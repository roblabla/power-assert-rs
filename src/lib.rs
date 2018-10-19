#![recursion_limit = "1024"]
#![feature(proc_macro_span)]

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unused_results)]

#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro2;
extern crate proc_macro;

use proc_macro::TokenStream;

mod expand;
mod convert;

#[proc_macro]
pub fn power_assert(input: TokenStream) -> TokenStream {
     expand::expand_assert(input)
}

#[proc_macro]
pub fn power_assert_eq(input: TokenStream) -> TokenStream {
    expand::expand_assert_eq(input)
}

#[cfg(feature = "override_globals")]
#[proc_macro]
pub fn assert(input: TokenStream) -> TokenStream {
    expand::expand_assert(input)
}

#[cfg(feature = "override_globals")]
#[proc_macro]
pub fn assert_eq(input: TokenStream) -> TokenStream {
    expand::expand_assert_eq(input)
}
