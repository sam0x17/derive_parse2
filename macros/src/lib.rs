use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced, bracketed, parenthesized,
    parse::Nothing,
    parse2, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{self, Bracket, Paren},
    Attribute, Error, Expr, Generics, Ident, ItemEnum, ItemStruct, Path, PathArguments, Result,
    Stmt, Token, Type, TypePath, Visibility,
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

impl quote::ToTokens for DeriveParseAttr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            DeriveParseAttr::Brace => tokens.extend(quote!(#[brace])),
            DeriveParseAttr::Bracket => tokens.extend(quote!(#[bracket])),
            DeriveParseAttr::Paren => tokens.extend(quote!(#[paren])),
            DeriveParseAttr::Inside(content) => tokens.extend(quote!(#[inside(#content)])),
            DeriveParseAttr::Call(content) => tokens.extend(quote!(#[call(#content)])),
            DeriveParseAttr::ParseIf(content) => tokens.extend(quote!(#[parse_if(#content)])),
            DeriveParseAttr::Prefix(content) => tokens.extend(quote!(#[prefix(#content)])),
            DeriveParseAttr::Postfix(content) => tokens.extend(quote!(#[postfix(#content)])),
        }
    }
}

impl DeriveParseAttr {
    fn tt_type(&self) -> Option<TokenTreeType> {
        match &self {
            DeriveParseAttr::Brace => Some(TokenTreeType::Brace),
            DeriveParseAttr::Bracket => Some(TokenTreeType::Bracket),
            DeriveParseAttr::Paren => Some(TokenTreeType::Paren),
            _ => None,
        }
    }
}

enum TokenTreeType {
    Brace,
    Bracket,
    Paren,
}

struct FieldDef {
    attrs: Vec<Attribute>,
    dp_attrs: Vec<DeriveParseAttr>,
    name: Ident,
    _colon: Token![:],
    typ: Type,
    optional: bool,
    parse_type: Type,
    tt: Option<TokenTreeType>,
    inside: Option<Ident>,
    call: Option<Expr>,
    parse_if: Option<Expr>,
    prefix: Option<Expr>,
    postfix: Option<Expr>,
}

impl syn::parse::Parse for FieldDef {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;
        let mut dp_attrs: Vec<DeriveParseAttr> = Vec::new();
        let mut attr_tt: Option<&DeriveParseAttr> = None;
        let mut inside: Option<Ident> = None;
        let mut call: Option<Expr> = None;
        let mut parse_if: Option<Expr> = None;
        let mut prefix: Option<Expr> = None;
        let mut postfix: Option<Expr> = None;
        attrs = attrs
            .into_iter()
            .filter(|attr| {
                if let Ok(dp_attr) = parse2::<DeriveParseAttr>(attr.to_token_stream()) {
                    dp_attrs.push(dp_attr);
                    return false;
                }
                true
            })
            .collect();
        let (mut has_tt, mut has_inside, mut has_call, mut has_parse_if, mut has_fix) =
            (false, false, false, false, false);
        for dp_attr in &dp_attrs {
            match dp_attr {
                DeriveParseAttr::Brace | DeriveParseAttr::Bracket | DeriveParseAttr::Paren => {
                    if has_tt {
                        return Err(Error::new(
                            dp_attr.span(),
                            "Only one of #[paren]/#[brace]/#[bracket] can be applied to a single field"
                        ));
                    }
                    attr_tt = Some(dp_attr);
                    has_tt = true;
                }
                DeriveParseAttr::Inside(ident) => {
                    if has_inside {
                        return Err(Error::new(
                            dp_attr.span(),
                            "Only one of #[inside(..)] can be applied to a single field",
                        ));
                    }
                    has_inside = true;
                    inside = Some(ident.clone());
                }
                DeriveParseAttr::Call(expr) => {
                    if has_call {
                        return Err(Error::new(
                            dp_attr.span(),
                            "Only one of #[call(..)] can be applied to a single field",
                        ));
                    }
                    has_call = true;
                    call = Some(expr.clone());
                }
                DeriveParseAttr::ParseIf(expr) => {
                    if has_parse_if {
                        return Err(Error::new(
                            dp_attr.span(),
                            "Only one of #[parse_if(..)] can be applied to a single field",
                        ));
                    }
                    has_parse_if = true;
                    parse_if = Some(expr.clone());
                }
                DeriveParseAttr::Prefix(_) | DeriveParseAttr::Postfix(_) => {
                    if has_fix {
                        return Err(Error::new(
                            dp_attr.span(),
                            "Only one of #[prefix(..)]/#[postfix(..)] can be applied to a single field"
                        ));
                    }
                    has_fix = true;
                    match dp_attr {
                        DeriveParseAttr::Prefix(expr) => prefix = Some(expr.clone()),
                        DeriveParseAttr::Postfix(expr) => postfix = Some(expr.clone()),
                        _ => unreachable!(),
                    }
                }
            }
        }
        if dp_attrs.len() > 2 {
            return Err(Error::new(
                dp_attrs[2].span(),
                "At most two derive_parse2 attribute helpers can be applied to a single struct field"
            ));
        }

        // parse field body
        let name = input.parse()?;
        let _colon = input.parse()?;
        let typ: Type = input.parse()?;

        let (optional, parse_type, parse_type_trailing_ident) = match typ.clone() {
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
                            "`Option<T>` must have exactly one generic argument",
                        ));
                    };
                    let inner_arg = inner_args.args.first().unwrap();
                    (true, parse_quote!(#inner_arg), {
                        if let Ok(inner_arg_path) = parse2::<Path>(inner_arg.to_token_stream()) {
                            if let Some(seg) = inner_arg_path.segments.last() {
                                Some(seg.ident.clone())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                } else {
                    (false, typ.clone(), Some(last_seg.ident.clone()))
                }
            }
            _ => {
                return Err(Error::new(
                    typ.span(),
                    "This type is not supported by derive_parse2::Parse",
                ))
            }
        };

        // shortcut to omit `#[brace]` / `#[bracket]` / `#[paren]` if using types that follow
        // syn's naming conventions
        if dp_attrs.len() < 2 && !has_tt {
            if let Some(ident) = parse_type_trailing_ident {
                let dp_attr = match ident.to_string().as_str() {
                    "Brace" => Some(DeriveParseAttr::Brace),
                    "Bracket" => Some(DeriveParseAttr::Bracket),
                    "Paren" => Some(DeriveParseAttr::Paren),
                    _ => None,
                };
                if let Some(dp_attr) = dp_attr {
                    dp_attrs.push(dp_attr.clone());
                    attr_tt = Some(&dp_attrs.iter().last().unwrap());
                }
            }
        }

        let tt = match attr_tt {
            Some(attr) => attr.tt_type(),
            None => None,
        };

        Ok(FieldDef {
            attrs,
            dp_attrs,
            name,
            _colon,
            typ,
            optional,
            parse_type,
            tt,
            inside,
            call,
            parse_if,
            prefix,
            postfix,
        })
    }
}

struct StructDef {
    attrs: Vec<Attribute>,
    vis: Visibility,
    _struct: Token![struct],
    ident: Ident,
    generics: Generics,
    _brace: token::Brace,
    fields: Punctuated<FieldDef, Token![,]>,
    _semi: Option<Token![;]>,
}

impl syn::parse::Parse for StructDef {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let content;
        Ok(StructDef {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            _struct: input.parse()?,
            ident: input.parse()?,
            generics: input.parse()?,
            _brace: braced!(content in input),
            fields: content.parse_terminated(FieldDef::parse, Token![,])?,
            _semi: match input.peek(Token![;]) {
                true => Some(input.parse()?),
                false => None,
            },
        })
    }
}

fn _derive_parse(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let tokens = tokens.into();
    if let Ok(_) = parse2::<ItemStruct>(tokens.clone()) {
        return derive_parse_struct(parse2::<StructDef>(tokens)?);
    }
    let item_enum = parse2::<ItemEnum>(tokens)?;
    derive_parse_enum(item_enum)
}

fn get_content_var(n: usize) -> Ident {
    format_ident!("content_{}", n)
}

fn derive_parse_struct(struct_def: StructDef) -> Result<TokenStream2> {
    let krate = quote!(::derive_parse2);
    let private = quote!(#krate::__private);
    let syn_ = quote!(#private::syn);
    let sa_ = quote!(#private::static_assertions);
    let quote_ = quote!(#private::quote);
    let mut derive_lines: Vec<TokenStream2> = Vec::new();
    Ok(quote!())
}

fn derive_parse_struct_old(item_struct: ItemStruct) -> Result<TokenStream2> {
    let mut assertions: Vec<TokenStream2> = Vec::new();
    let krate = quote!(::derive_parse2);
    let private = quote!(#krate::__private);
    let syn_ = quote!(#private::syn);
    let sa_ = quote!(#private::static_assertions);
    let quote_ = quote!(#private::quote);
    let mut derive_lines: Vec<TokenStream2> = Vec::new();
    let mut inside_stack: Vec<(Ident, Ident)> = Vec::new();
    let mut content_vars: Vec<Ident> = Vec::new();
    let mut content_var: Option<Ident> = None;
    let mut inside_of: Option<Ident> = None;

    for mut field in item_struct.fields {
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
                    let inner_arg_ident = parse2::<Ident>(inner_arg.to_token_stream()).ok();
                    (true, parse_quote!(#inner_arg), inner_arg_ident)
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

        let mut process_field = |cvar: Option<Ident>| {
            let parse_stream = match cvar {
                Some(ident) => ident,
                None => parse_quote!(input),
            };
            if field_is_optional {
                derive_lines.push(quote! {
                    #field_ident: match #parse_stream.parse::<#field_type>() {
                        Ok(res) => Some(res),
                        Err(_) => None,
                    }
                });
            } else {
                derive_lines.push(quote!(#field_ident: #parse_stream.parse::<#field_type>()?));
            }
        };

        field.attrs.clear();

        match attr {
            None => {
                // parsing for a "normal" field
                process_field(None);
            }
            Some(DeriveParseAttr::Brace)
            | Some(DeriveParseAttr::Bracket)
            | Some(DeriveParseAttr::Paren) => {
                let mut prev_content_var: Ident = parse_quote!(input);
                if let (Some(content_var), Some(inside_of)) = (content_var, inside_of) {
                    // if we are already inside of something, push it onto the stack so we can
                    // return to it once we are done with this token tree
                    inside_stack.push((content_var.clone(), inside_of));
                    prev_content_var = content_var;
                }
                content_var = Some(get_content_var(content_vars.len()));
                inside_of = Some(field_ident.clone());
                content_vars.push(content_var.clone().unwrap());

                // get the syn parse helper for this token tree
                let parse_helper = match attr.unwrap() {
                    DeriveParseAttr::Brace => quote!(braced!),
                    DeriveParseAttr::Bracket => quote!(bracketed!),
                    DeriveParseAttr::Paren => quote!(parenthesized!),
                    _ => unreachable!(),
                };

                if field_is_optional {
                    // note: only will work with peek
                    unimplemented!()
                } else {
                    derive_lines.push(quote! {
                        #field_ident: #syn_::#parse_helper(#content_var in #prev_content_var)
                    });
                }
            }
            Some(DeriveParseAttr::Inside(inside_of_ident)) => {
                while Some(inside_of_ident.clone()) != inside_of {
                    // this doesn't match the current inside of, we should pop
                    (content_var, inside_of) = match inside_stack.pop() {
                        Some((content, inside)) => (Some(content), Some(inside)),
                        None => return Err(Error::new(
                            inside_of_ident.span(),
                            "The specified token tree doesn't exist or has already been parsed fully"
                        )),
                    };
                }
                process_field(content_var.clone());
            }
            _ => unimplemented!(),
        }
    }
    let struct_ident = item_struct.ident;
    let struct_generics = item_struct.generics;
    let output = quote! {
        impl #struct_generics #syn_::parse::Parse for #struct_ident #struct_generics {
            fn parse(input: #syn_::parse::ParseStream) -> #syn_::Result<Self> {
                #(let #content_vars;)
                *
                Ok(#struct_ident {
                    #(#derive_lines),
                    *
                })
            }
        }
    };
    // println!("{}", output.to_string());
    Ok(output)
}

fn derive_parse_enum(_item_enum: ItemEnum) -> Result<TokenStream2> {
    Ok(quote!())
}

#[proc_macro_derive(
    Parse,
    attributes(
        paren,
        bracket,
        brace,
        inside,
        call,
        parse_terminated,
        peek,
        peek_with,
        parse_if,
        prefix,
        postfix,
    )
)]
pub fn derive_parse(tokens: TokenStream) -> TokenStream {
    match _derive_parse(tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[cfg(test)]
mod tests;
