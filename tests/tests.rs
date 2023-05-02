use derive_parse2::*;
use quote::{quote, ToTokens};
use syn::{parse2, token, Ident, Token};

#[derive(Parse)]
pub struct BasicTypeUse {
    _use: Token![use],
    typ: syn::Type,
    _as: syn::Token![as],
    alias: Ident,
}

#[test]
fn generated_struct_parsing_basic_type_use_valid() {
    let res = parse2::<BasicTypeUse>(quote!(use something::MyCoolType as CoolIdent)).unwrap();
    assert_eq!(
        res.typ.to_token_stream().to_string(),
        "something :: MyCoolType"
    );
    assert_eq!(res.alias.to_token_stream().to_string(), "CoolIdent");

    parse2::<BasicTypeUse>(quote!(use even::longer::Path as CoolIdent)).unwrap();
    parse2::<BasicTypeUse>(quote!(use _ as CoolIdent)).unwrap();
}

#[test]
fn generated_struct_parsing_basic_type_use_invalid() {
    assert!(parse2::<BasicTypeUse>(quote!(use something::MyCoolType a CoolIdent)).is_err());
    assert!(parse2::<BasicTypeUse>(quote!(use something::MyCoolType as some::path)).is_err());
    assert!(parse2::<BasicTypeUse>(quote!(something::MyCoolType as CoolIdent)).is_err());
    assert!(parse2::<BasicTypeUse>(quote!(use something::MyCoolType as as CoolIdent)).is_err());
    assert!(parse2::<BasicTypeUse>(quote!(use use something::MyCoolType as CoolIdent)).is_err());
}

#[derive(Parse)]
pub struct FullTypeUse {
    _pub: Option<Token![pub]>,
    _use: Token![use],
    typ: syn::Type,
    _as: syn::Token![as],
    alias: Ident,
}

#[test]
fn generated_struct_parsing_full_type_use_valid() {
    let res = parse2::<FullTypeUse>(quote!(use something::MyCoolType as CoolIdent)).unwrap();
    assert_eq!(
        res.typ.to_token_stream().to_string(),
        "something :: MyCoolType"
    );
    assert_eq!(res.alias.to_token_stream().to_string(), "CoolIdent");

    parse2::<FullTypeUse>(quote!(use even::longer::Path as CoolIdent)).unwrap();
    parse2::<FullTypeUse>(quote!(use _ as CoolIdent)).unwrap();
    parse2::<FullTypeUse>(quote!(use _ as CoolIdent)).unwrap();
    parse2::<FullTypeUse>(quote!(pub use SomeType as CoolIdent)).unwrap();
}

#[test]
fn generated_struct_parsing_full_type_use_invalid() {
    assert!(parse2::<FullTypeUse>(quote!(use something::MyCoolType a CoolIdent)).is_err());
    assert!(parse2::<FullTypeUse>(quote!(use something::MyCoolType as some::path)).is_err());
    assert!(parse2::<FullTypeUse>(quote!(something::MyCoolType as CoolIdent)).is_err());
    assert!(parse2::<FullTypeUse>(quote!(use something::MyCoolType as as CoolIdent)).is_err());
    assert!(parse2::<FullTypeUse>(quote!(use use something::MyCoolType as CoolIdent)).is_err());
    assert!(parse2::<FullTypeUse>(quote!(use pub something::MyCoolType as CoolIdent)).is_err());
}

pub mod keywords {
    use syn::custom_keyword;

    custom_keyword!(IdentsList);
    custom_keyword!(yeah);
}

#[derive(Parse)]
pub struct ThreeIdentsList {
    _args_list: keywords::IdentsList,
    #[paren]
    _paren: token::Paren,
    #[inside(_paren)]
    ident1: Ident,
    #[inside(_paren)]
    _comma1: token::Comma,
    #[inside(_paren)]
    ident2: Ident,
    #[inside(_paren)]
    _comma2: token::Comma,
    #[inside(_paren)]
    ident3: Ident,
    #[inside(_paren)]
    _comma3: Option<token::Comma>,
    _colons: Token![::],
    _yeah: keywords::yeah,
}

#[test]
fn generated_struct_parsing_three_idents_list_valid() {
    let res = parse2::<ThreeIdentsList>(quote!(IdentsList(one, two, three)::yeah)).unwrap();
    assert_eq!(res.ident1.to_token_stream().to_string(), "one");
    assert_eq!(res.ident2.to_token_stream().to_string(), "two");
    assert_eq!(res.ident3.to_token_stream().to_string(), "three");

    parse2::<ThreeIdentsList>(quote!(IdentsList(one, two, three,)::yeah)).unwrap();
}

#[test]
fn generated_struct_parsing_three_idents_list_invalid() {
    assert!(parse2::<ThreeIdentsList>(quote!(IdentsList(one, two, three,)::)).is_err());
    assert!(parse2::<ThreeIdentsList>(quote!(IdentsList(one, two, three, four)::yeah)).is_err());
    assert!(parse2::<ThreeIdentsList>(quote!(IdentsList(one, two, three)yeah)).is_err());
}
