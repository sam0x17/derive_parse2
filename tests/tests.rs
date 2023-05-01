use derive_parse2::*;
use quote::{quote, ToTokens};
use syn::{parse2, Ident, Token};

#[derive(Parse)]
pub struct BasicTypeUse {
    _use: Token![use],
    typ: syn::Type,
    _as: syn::Token![as],
    alias: Ident,
}

#[test]
fn generated_struct_parsing_basic_type_use_valid() {
    let tokens = quote!(use something::MyCoolType as CoolIdent);
    let res = parse2::<BasicTypeUse>(tokens).unwrap();
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
