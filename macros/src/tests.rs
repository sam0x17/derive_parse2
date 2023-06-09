use super::*;

#[test]
fn derive_parse_attr_bracket_valid() {
    let tokens = quote!(#[bracket]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Bracket,
    ));
}

#[test]
fn derive_parse_attr_bracket_invalid() {
    let tokens = quote!(#[bracket = something]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_paren_valid() {
    let tokens = quote!(#[paren]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Paren,
    ));
}

#[test]
fn derive_parse_attr_paren_invalid() {
    let tokens = quote!(#[paren(something)]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_brace_valid() {
    let tokens = quote!(#[brace]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Brace,
    ));
}

#[test]
fn derive_parse_attr_brace_invalid() {
    let tokens = quote!(#[brace!]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_inside_valid() {
    let tokens = quote!(#[inside(some_ident)]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Inside(_),
    ));
}

#[test]
fn derive_parse_attr_inside_invalid() {
    let tokens = quote!(#[inside[some_ident]]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[inside some_ident]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[inside]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[inside()]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_call_valid() {
    let tokens = quote!(#[call(some_ident.some_func())]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Call(_),
    ));
}

#[test]
fn derive_parse_attr_call_invalid() {
    let tokens = quote!(#[call[something]]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[call some_ident]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[call]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[call()]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_parse_if_valid() {
    let tokens = quote!(#[parse_if(some_ident.some_func())]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::ParseIf(_),
    ));
}

#[test]
fn derive_parse_attr_parse_if_invalid() {
    let tokens = quote!(#[parse_if[something]]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[parse_if some_ident]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[parse_if]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[parse_if()]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_prefix_valid() {
    let tokens = quote!(#[prefix(some_ident.some_func())]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Prefix(_),
    ));
}

#[test]
fn derive_parse_attr_prefix_invalid() {
    let tokens = quote!(#[prefix[something]]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[prefix some_ident]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[prefix]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[prefix()]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn derive_parse_attr_postfix_valid() {
    let tokens = quote!(#[postfix(some_ident.some_func())]);
    assert!(matches!(
        parse2::<DeriveParseAttr>(tokens).unwrap(),
        DeriveParseAttr::Postfix(_),
    ));
}

#[test]
fn derive_parse_attr_postfix_invalid() {
    let tokens = quote!(#[postfix[something]]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[postfix some_ident]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[postfix]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
    let tokens = quote!(#[postfix()]);
    assert!(parse2::<DeriveParseAttr>(tokens).is_err());
}

#[test]
fn test_derive_parse_struct_basic_types() {
    let tokens = quote! {
        struct MyTypeUse {
            _use: Token![use],
            typ: syn::Type,
            _as: syn::Token![as],
            alias: Ident,
        }
    };
    derive_parse_struct(parse_quote!(#tokens)).unwrap();
}

#[test]
fn test_parse_field_def_valid() {
    parse2::<FieldDef>(quote!(some_ident: bool)).unwrap();
    parse2::<FieldDef>(quote!(ident: Something)).unwrap();
    parse2::<FieldDef>(quote!(my_field: some::typ::Path)).unwrap();
    parse2::<FieldDef>(quote! {
        #[paren]
        _paren: syn::token::Paren
    })
    .unwrap();
}

#[test]
fn test_parse_field_def_invalid() {
    assert!(parse2::<FieldDef>(quote!(some_ident bool)).is_err());
    assert!(parse2::<FieldDef>(quote!(Something<Thing>: bool)).is_err());
    assert!(parse2::<FieldDef>(quote!(some::thing: bool)).is_err());
}

#[test]
fn test_struct_def_parsing_valid() {
    parse2::<StructDef>(quote! {
        #[some_attribute]
        pub struct MyStruct {
            some_field: syn::token::Comma,
            some_other_field: Token![;],
            yet_another_field: Option<SomeType>,
        }
    })
    .unwrap();
    parse2::<StructDef>(quote! {
        #[some_attribute]
        #[another(attribute)]
        struct MyStruct {
            some_field: syn::token::Comma
        }
    })
    .unwrap();
    parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma
        };
    })
    .unwrap();
    parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma,
            #[paren]
            #[inside(something)]
            another_field: Something
        }
    })
    .unwrap();
}

#[test]
fn test_struct_def_parsing_invalid() {
    assert!(parse2::<StructDef>(quote! {
        enum MyStruct {
            some_field: syn::token::Comma
        }
    })
    .is_err());
    assert!(parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma
            another_field: Something
        }
    })
    .is_err());
    assert!(parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma,
            #[paren]
            #[bracket]
            #[inside(something)]
            another_field: Something
        }
    })
    .is_err());
    assert!(parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma,
            #[paren]
            #[bracket]
            another_field: Something
        }
    })
    .is_err());
    assert!(parse2::<StructDef>(quote! {
        struct MyStruct {
            some_field: syn::token::Comma,
            #[paren]
            #[paren]
            another_field: Something
        }
    })
    .is_err());
}
