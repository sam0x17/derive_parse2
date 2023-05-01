pub use derive_parse2_macros::*;

#[doc(hidden)]
pub mod __private {
    pub use quote;
    pub use static_assertions;
    pub use syn;
}
