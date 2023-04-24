use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    bracketed, parenthesized,
    parse::Nothing,
    parse2,
    token::{Bracket, Paren},
    Error, Expr, Ident, ItemEnum, ItemStruct, Result, Token,
};

fn _derive_parse(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let tokens = tokens.into();
    if let Ok(item_struct) = parse2::<ItemStruct>(tokens.clone()) {
        return derive_parse_struct(item_struct);
    }
    let item_enum = parse2::<ItemEnum>(tokens)?;
    derive_parse_enum(item_enum)
}

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

fn derive_parse_struct(_item_struct: ItemStruct) -> Result<TokenStream2> {
    Ok(quote!())
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
