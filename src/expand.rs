use convert;
use syn::{self, Stmt, Expr, Ident};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use proc_macro2::Span;
use proc_macro::TokenStream;
use quote::ToTokens;

//fn filter_tts_by_span(span: Span, tts: &[TokenTree]) -> Vec<TokenTree> {
//    tts.iter()
//       .filter(|tt| span.lo <= tt.get_span().lo && tt.get_span().hi <= span.hi)
//       .cloned()
//       .collect()
//}

fn expr_prelude() -> Vec<Stmt> {
    let use_stmt: Stmt = parse_quote!(use std::io::Write;);
    let fn_stmt: Stmt = parse_quote!(
        fn inspect<T: Write>(writer: &mut T, mut vals: Vec<(usize, String)>, offset: usize, repr: &str) {
            fn width(s: &str) -> usize { s.len() }
            fn align<T: Write>(writer: &mut T, cur: &mut usize, col: usize, s: &str) {
                while *cur < col {
                    let _ = write!(writer, " ");
                    *cur += 1;
                }
                let _ = write!(writer, "{}", s);
                *cur += width(s);
            }

            vals.sort();

            let _ = writeln!(writer, "{}", repr);
            {
                let mut cur = 0;
                for &(c, _) in &vals {
                    align(writer, &mut cur, offset + c, "|");
                }
                let _ = writeln!(writer, "");
            }
            while !vals.is_empty() {
                let mut cur = 0;
                let mut i = 0;
                while i < vals.len() {
                    if i == vals.len() - 1 ||
                        vals[i].0 + width(&vals[i].1) < vals[i + 1].0 {
                            align(writer, &mut cur, offset + vals[i].0, &vals[i].1);
                            let _ = vals.remove(i);
                        } else {
                            align(writer, &mut cur, offset + vals[i].0, "|");
                            i += 1;
                        }
                }
                let _ = writeln!(writer, "");
            }
        });
    let write_stmt: Stmt = parse_quote!(let mut write_buf = vec![];);

    vec![use_stmt, fn_stmt, write_stmt]
}

struct ExprGen {
    ident: Ident,
    expr: Expr,
    ppstr: TokenStream
}

impl ExprGen {
    fn new(ident: &str, expr: Expr) -> ExprGen {
        let ppstr = expr.clone().into_token_stream();

        ExprGen {
            ident: Ident::new(ident, Span::call_site()),
            expr: expr,
            ppstr: ppstr.into(),
        }
    }

    fn ppstr(&self) -> &TokenStream {
        &self.ppstr
    }

    fn init_stmt(&self, pushed: bool) -> Stmt {
        let ident = self.ident.clone();
        if pushed {
            parse_quote!(let mut #ident = vec![];)
        } else {
            parse_quote!(let #ident = vec![];)
        }
    }

    fn converted_expr(&self) -> (Expr, bool) {
        convert::convert_expr(self.expr.clone(), self.ident.clone())
    }

    fn inspect_expr(&self, left: &str, right: &str) -> Expr {
        let offset = left.len();
        let repr = format!("{}{}{}", left, self.ppstr, right);
        let ident = &self.ident;
        parse_quote!(inspect(&mut write_buf, #ident, #offset, #repr))
    }
}

struct Args {
    fields: Punctuated<Expr, Token![,]>
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::parse::Result<Args> {
        Ok(Args {
            fields: input.parse_terminated(Expr::parse)?
        })
    }
}

pub fn expand_assert(s: TokenStream) -> TokenStream {
    let values = parse_macro_input!(s as Args);

    let mut iter = values.fields.into_iter();
    let cond_expr = iter.next().expect("At least one value in power_assert!");
    let msg_tts = iter.next();

    let prelude = expr_prelude();

    let gen = ExprGen::new("vals", cond_expr);
    let inspect = gen.inspect_expr("power_assert!(", ")");
    let (converted, pushed) = gen.converted_expr();
    let init = gen.init_stmt(pushed);

    let panic_msg : Expr = if let Some(tts) = msg_tts {
        parse_quote!(format!(#tts))
    } else {
        let msg = format!("assertion failed: {}", gen.ppstr());
        parse_quote!(#msg)
    };

    let expr : Expr = parse_quote!({
        #init;
        let cond = #converted;
        if !cond {
            #(#prelude;)*
            let _ = writeln!(&mut write_buf, #panic_msg);
            #inspect;
            panic!(String::from_utf8(write_buf).unwrap())
        }
    });

    //println!("{}", expr.clone().into_token_stream());

    expr.into_token_stream().into()
}

pub fn expand_assert_eq(s: TokenStream) -> TokenStream {
    let values = parse_macro_input!(s as Args);

    let mut iter = values.fields.into_iter();
    let lhs = iter.next().expect("At least an lhs and an rhs in power_assert_eq!");
    let rhs = iter.next().expect("At least an lhs and an rhs in power_assert_eq!");

    let lhs_gen = ExprGen::new("lhs_vals", lhs);
    let rhs_gen = ExprGen::new("rhs_vals", rhs);

    let prelude = expr_prelude();

    let lhs_inspect = lhs_gen.inspect_expr("left: ", "");
    let rhs_inspect = rhs_gen.inspect_expr("right: ", "");
    let (lhs_converted, lhs_pushed) = lhs_gen.converted_expr();
    let (rhs_converted, rhs_pushed) = rhs_gen.converted_expr();
    let lhs_init = lhs_gen.init_stmt(lhs_pushed);
    let rhs_init = rhs_gen.init_stmt(rhs_pushed);
    let assert_msg = format!("power_assert_eq!({}, {})", lhs_gen.ppstr(), rhs_gen.ppstr());

    let expr: Expr = parse_quote!({
        #lhs_init;
        #rhs_init;
        match (&#lhs_converted, &#rhs_converted) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    #(#prelude;)*
                    let _ = writeln!(&mut write_buf, "assertion failed: `(left == right)` \
                            (left: `{:?}`, right: `{:?}`)", *left_val, *right_val);
                    let _ = writeln!(&mut write_buf, #assert_msg);
                    #lhs_inspect;
                    #rhs_inspect;
                    panic!(String::from_utf8(write_buf).unwrap());
                }
            }
        }
    });

    //println!("{}", expr.clone().into_token_stream());

    expr.into_token_stream().into()
}
