use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    bracketed, parenthesized,
    parse::Nothing,
    parse2, parse_quote,
    spanned::Spanned,
    token::{Bracket, Paren},
    Error, Expr, Ident, ItemEnum, ItemStruct, PathArguments, Result, Token, Type, TypePath,
};

mod keywords {
    use syn::custom_keyword;

    custom_keyword!(brace);
    custom_keyword!(bracket);
    custom_keyword!(paren);
    custom_keyword!(inside);
    custom_keyword!(call);
    custom_keyword!(parse_if);
    custom_keyword!(prefix);
    custom_keyword!(postfix);
}

#[derive(Clone)]
enum DeriveParseAttr {
    Brace,
    Bracket,
    Paren,
    Inside(Ident),
    Call(Expr),
    ParseIf(Expr),
    Prefix(Expr),
    Postfix(Expr),
}

// impl PartialEq for DeriveParseAttr {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Inside(l0), Self::Inside(r0)) => l0.to_string() == r0.to_string(),
//             (Self::Call(l0), Self::Call(r0)) => {
//                 l0.to_token_stream().to_string() == r0.to_token_stream().to_string()
//             }
//             (Self::ParseIf(l0), Self::ParseIf(r0)) => {
//                 l0.to_token_stream().to_string() == r0.to_token_stream().to_string()
//             }
//             (Self::Prefix(l0), Self::Prefix(r0)) => {
//                 l0.to_token_stream().to_string() == r0.to_token_stream().to_string()
//             }
//             (Self::Postfix(l0), Self::Postfix(r0)) => {
//                 l0.to_token_stream().to_string() == r0.to_token_stream().to_string()
//             }
//             _ => core::mem::discriminant(self) == core::mem::discriminant(other),
//         }
//     }
// }

impl syn::parse::Parse for DeriveParseAttr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let content;
        let _pound_token: Token![#] = input.parse()?;
        let _bracket_token: Bracket = bracketed!(content in input);
        if content.peek(keywords::brace) {
            content.parse::<keywords::brace>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Brace)
        } else if content.peek(keywords::bracket) {
            content.parse::<keywords::bracket>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Bracket)
        } else if content.peek(keywords::paren) {
            content.parse::<keywords::paren>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Paren)
        } else if content.peek(keywords::inside) {
            content.parse::<keywords::inside>()?;
            let paren_content;
            let _paren_token: Paren = parenthesized!(paren_content in content);
            let ident = paren_content.parse::<Ident>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Inside(ident))
        } else if content.peek(keywords::call) {
            content.parse::<keywords::call>()?;
            let paren_content;
            let _paren_token: Paren = parenthesized!(paren_content in content);
            let expr = paren_content.parse::<Expr>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Call(expr))
        } else if content.peek(keywords::parse_if) {
            content.parse::<keywords::parse_if>()?;
            let paren_content;
            let _paren_token: Paren = parenthesized!(paren_content in content);
            let expr = paren_content.parse::<Expr>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::ParseIf(expr))
        } else if content.peek(keywords::prefix) {
            content.parse::<keywords::prefix>()?;
            let paren_content;
            let _paren_token: Paren = parenthesized!(paren_content in content);
            let expr = paren_content.parse::<Expr>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Prefix(expr))
        } else if content.peek(keywords::postfix) {
            content.parse::<keywords::postfix>()?;
            let paren_content;
            let _paren_token: Paren = parenthesized!(paren_content in content);
            let expr = paren_content.parse::<Expr>()?;
            content.parse::<Nothing>()?;
            Ok(DeriveParseAttr::Postfix(expr))
        } else {
            return Err(Error::new(
                content.span(),
                "Expected one of `brace`, `bracket`, `paren`,\
                `inside`, `call`, `parse_if`, `prefix`, `postfix`.",
            ));
        }
    }
}

fn _derive_parse(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let tokens = tokens.into();
    if let Ok(item_struct) = parse2::<ItemStruct>(tokens.clone()) {
        return derive_parse_struct(item_struct);
    }
    let item_enum = parse2::<ItemEnum>(tokens)?;
    derive_parse_enum(item_enum)
}

fn derive_parse_struct(item_struct: ItemStruct) -> Result<TokenStream2> {
    let mut assertions: Vec<TokenStream2> = Vec::new();
    let krate = quote!(::derive_parse2);
    let private = quote!(#krate::__private);
    let syn_ = quote!(#private::syn);
    let sa_ = quote!(#private::static_assertions);
    let quote_ = quote!(#private::quote);
    let mut derive_lines: Vec<TokenStream2> = Vec::new();
    for field in item_struct.fields {
        if field.attrs.len() > 1 {
            return Err(Error::new(
                field.attrs[1].span(),
                "Only one attribute is supported per struct item.",
            ));
        }

        let (field_is_optional, field_type, field_type_ident) = match field.ty.clone() {
            Type::Macro(mac) => (false, Type::Macro(mac), None),
            Type::Path(TypePath { path, qself: _ }) => {
                let Some(last_seg) = path.segments.last() else {
                    return Err(Error::new(path.span(), "Path must have at least one segment."));
                };

                if last_seg.ident == "Option" {
                    let PathArguments::AngleBracketed(inner_args) = last_seg.arguments.clone() else {
                        return Err(Error::new(last_seg.span(), "Only an `Option<T>` or `T` are supported in this context where `T: syn::Parse`"));
                    };
                    if inner_args.args.len() != 1 {
                        return Err(Error::new(
                            inner_args.span(),
                            "`Option<T>` only supports one generic argument",
                        ));
                    };
                    let inner_arg = inner_args.args.first().unwrap();
                    assertions.push(
                        quote!(#sa_::assert_impl_all!(#inner_arg: #syn_::parse::Parse, #quote_::ToTokens);),
                    );
                    let inner_arg_ident: Ident = parse_quote!(#inner_arg);
                    (true, parse_quote!(#inner_arg), Some(inner_arg_ident))
                } else {
                    assertions.push(
                        quote!(#sa_::assert_impl_all!(#path: #syn_::parse::Parse, #quote_::ToTokens);),
                    );
                    (false, field.ty.clone(), Some(last_seg.ident.clone()))
                }
            }
            _ => {
                return Err(Error::new(
                    field.ty.span(),
                    "This type is not supported by derive_parse2::Parse",
                ))
            }
        };

        // shortcut to omit `#[brace]` / `#[bracket]` / `#[paren]` if using types that follow
        // syn's naming conventions
        let attr_according_to_field = match field_type_ident {
            None => None,
            Some(ident) => match ident.to_string().as_str() {
                "Brace" => Some(DeriveParseAttr::Brace),
                "Bracket" => Some(DeriveParseAttr::Bracket),
                "Paren" => Some(DeriveParseAttr::Paren),
                _ => None,
            },
        };

        // parse attr or use default based on field type (or None)
        let attr = field.attrs.first();
        let (_attr_span, attr) = match attr {
            Some(attr) => (
                attr.span(),
                Some(parse2::<DeriveParseAttr>(attr.to_token_stream())?),
            ),
            None => (field.span(), attr_according_to_field),
        };
        let Some(field_ident) = field.ident else {
            return Err(Error::new(field.span(), "All fields in this struct must have a defined ident (field name)"));
        };

        match attr {
            None => {
                // parsing for a "normal" field
                match field_is_optional {
                    false => derive_lines.push(quote!(#field_ident: input.parse::<#field_type>()?)),
                    true => derive_lines.push(quote! {
                        #field_ident: match input.parse::<#field_type>()? {
                            Ok(res) => Some(res),
                            Err(_) => None,
                        },
                    }),
                }
            }
            Some(_attr) => {
                // parsing for a field with an attr or a brace/bracket/paren auto
                unimplemented!()
            }
        }
    }
    let struct_ident = item_struct.ident;
    let struct_generics = item_struct.generics;
    Ok(quote! {
        impl #struct_generics #syn_::parse::Parse for #struct_ident #struct_generics {
            fn parse(input: #syn_::parse::ParseStream) -> #syn_::Result<Self> {
                Ok(#struct_ident {
                    #(#derive_lines),*
                })
            }
        }
    })
}

fn derive_parse_enum(_item_enum: ItemEnum) -> Result<TokenStream2> {
    Ok(quote!())
}

#[proc_macro_derive(Parse)]
pub fn derive_parse(tokens: TokenStream) -> TokenStream {
    match _derive_parse(tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[cfg(test)]
mod tests;
