# power-assert-rs

[![Build Status](https://travis-ci.org/gifnksm/power-assert-rs.svg)](https://travis-ci.org/gifnksm/power-assert-rs)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![crates.io](http://meritbadge.herokuapp.com/power-assert)](https://crates.io/crates/power-assert)

Power Assert in Rust. Provides better assertion message like this:

```
$ cargo run --example normal
     Running `target/debug/examples/normal`
thread '<main>' panicked at 'assertion failed: bar.val == bar.foo.val
power_assert!(bar.val == bar.foo.val)
              |   |   |  |   |   |
              |   3   |  |   |   2
              |       |  |   Foo { val: 2 }
              |       |  Bar { val: 3, foo: Foo { val: 2 } }
              |       false
              Bar { val: 3, foo: Foo { val: 2 } }
', examples/normal.rs:26
An unknown error occurred

To learn more, run the command again with --verbose.
```

# Rust version

This plugin exposes a proc macro, and as such requires a nightly compiler. At least version 1.30 is necessary.

It relies on the following nightly features:

- `proc_macro_hygiene`
- `proc_macro_span`


# How to use

Add this to your `Cargo.toml`:

```toml
[dependencies]
power-assert = "*"
```

and add this to your `lib.rs` or `main.rs`:

```rust
#![feature(proc_macro_hygiene)]
#[macro_use]
extern crate power_assert;
```

Now, you can use `power_assert!()` and `power_assert_eq!()`.

If you want to override builtin `assert!()` and `assert_eq!()`, change your `Cargo.toml` as follows.

```toml
[dependencies]
power-assert = { version = "*", features = ["override_globals"] }
```

If you want to use power-assert in a `no_std` environment, then make sure you disable the `std` feature:

```toml
[dependencies]
power-assert = { version = "*", default-features = false }
```

Note that, in `no_std` environment, power_assert still needs an allocator (it uses Vec and the format macro).

# Issues

- 
