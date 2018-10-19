use syn::{self, *};
use syn::fold::Fold;
use syn::spanned::Spanned;
use proc_macro::Span;
use quote::ToTokens;

struct AssertFolder {
    ident: Ident,
    push_count: usize,
    startpos: usize,
}

impl Fold for AssertFolder {
    fn fold_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            // Expr::Box
            // Expr::InPlace
            // Expr::Array
            Expr::Call(mut exprcall) => {
                let col = exprcall.func.span().unstable().start().column - self.startpos;

                let newargs = exprcall.args.iter()
                    .map(|expr| self.fold_expr(expr.clone()))
                    .collect();

                exprcall.args = newargs;

                let ident = &self.ident;

                self.push_count += 1;
                parse_quote!({
                    let expr = #exprcall;
                    #ident.push((#col, format!("{:?}", expr)));
                    expr
                })
            },
            Expr::MethodCall(ExprMethodCall { ref receiver, .. }) => {
                let col = receiver.span().unstable().start().column - self.startpos;
                let conv_expr = syn::fold::fold_expr(self, expr.clone());
                let ident = &self.ident;
                self.push_count += 1;
                parse_quote!(self.cx, {
                    let expr = #conv_expr;
                    #ident.push((#col, format!("{:?}", expr)));
                    expr
                })
            }
            // Expr::Tuple
            Expr::Binary(ExprBinary { op, .. }) => {
                let col = op.span().unstable().start().column - self.startpos;
                let conv_expr = syn::fold::fold_expr(self, expr);
                let ident = &self.ident;
                self.push_count += 1;
                parse_quote!({
                    let expr = #conv_expr;
                    #ident.push((#col, format!("{:?}", expr)));
                    expr
                })
            }
            // Expr::Unary
            // Expr::Lit
            // Expr::Cast
            // Expr::Type
            // Expr::Let
            Expr::If(..) |
            Expr::Match(..) => {
                let col = expr.span().unstable().start().column - self.startpos;
                let conv_expr = syn::fold::fold_expr(self, expr);
                let ident = &self.ident;
                self.push_count += 1;
                parse_quote!({
                    let expr = #conv_expr;
                    #ident.push((#col, format!("{:?}", expr)));
                    expr
                })
            }
            // Expr::While
            // Expr::ForLoop
            // Expr::Loop
            // Expr::Match: See above
            // Expr::Closure
            // Expr::Unsafe
            // Expr::Block
            // Expr::Assign
            // Expr::AssignOp
            Expr::Field(field) => self.convert_field(field.clone()),
            Expr::Index(ExprIndex { .. }) => {
                // TODO: Fix column
                let col = expr.span().unstable().start().column - self.startpos;
                let conv_expr = syn::fold::fold_expr(self, expr.clone());
                let ident = &self.ident;
                self.push_count += 1;
                parse_quote!({
                    let expr = #conv_expr;
                    #ident.push((#col, format!("{:?}", expr)));
                    #expr
                })
            }
            // Expr::Range
            Expr::Path(..) => {
                let col = expr.span().unstable().start().column - self.startpos;
                let ident = &self.ident;
                self.push_count += 1;
                parse_quote!({
                    #ident.push((#col, format!("{:?}", #expr)));
                    #expr
                })
            }
            // Expr::Reference
            // Expr::Break
            // Expr::Continue
            // Expr::Return
            // Expr::Macro
            // Expr::Struct
            // Expr::Repeat
            // Expr::Paren
            // Expr::Group
            // Expr::Try
            // Expr::Async
            // Expr::TryBlock
            // Expr::Yield
            // Expr::Verbatim
            _ => syn::fold::fold_expr(self, expr),
        }
    }
}

impl AssertFolder {
    fn convert_field(&mut self, expr: ExprField) -> Expr {
        let mut exprs = vec![];
        let mut cur_expr = Expr::Field(expr.clone());
        let prelude: Stmt;
        let ident: Expr;
        loop {
            cur_expr = match cur_expr {
                Expr::Field(ExprField { ref base, .. }) => {
                    exprs.push(cur_expr.clone());
                    *base.clone()
                }
                Expr::Path(ExprPath { .. }) => {
                    ident = cur_expr.clone();
                    let id = &self.ident;
                    let col = expr.span().unstable().start().column - self.startpos;
                    self.push_count += 1;
                    prelude = parse_quote!(#id.push((#col, format!("{:?}", #ident))););
                    break;
                }
                _ => {
                    let id = Ident::new("a", Span::call_site().into());
                    ident = parse_quote!($id);
                    let conv_expr = syn::fold::fold_expr(self, cur_expr.clone());
                    prelude = parse_quote!(let #id = #conv_expr;);
                    break;
                }
            }
        }
        exprs.reverse();
        let exprs = exprs.into_iter()
                         .scan(ident, |st, item| {
                             let mut item = item.clone();
                             let col = match &mut item {
                                 Expr::Field(ref mut exprfield) => {
                                     let col = exprfield.member.span().unstable().start().column;
                                     exprfield.base = Box::new((*st).clone());
                                     col - self.startpos
                                 },
                                 _ => panic!(),
                             };
                             *st = item;
                             Some((st.clone(), col))
                         })
                         .collect::<Vec<_>>();
        let last_expr = exprs[exprs.len() - 1].0.clone();
        let mut stmts = vec![prelude.clone()];
        for (e, col) in exprs {
            let ident = &self.ident;
            self.push_count += 1;
            stmts.push(parse_quote!(#ident.push((#col, format!("{:?}", #e)));));
        }

        let ret : Expr = parse_quote!({
            #(#stmts)*
            #last_expr
        });
        ret
    }
}

pub fn convert_expr(expr: Expr,
                    ident: Ident)
                    -> (Expr, bool) {
    // Pretty-printed snippet is used for showing assersion failed message, but
    // its tokens may have different spans from original code snippet's tokens.
    // Power-assert using those spans, so, parse the pretty-printed snippet
    // again to get tokens that have the same spans with pretty-printed snippet.
    //let ppexpr_str = pprust::expr_to_string(&expr);
    //let pos_map = create_pos_map(cx, ppexpr_str, tts);

    // Convert
    let mut folder = AssertFolder {
        ident: ident,
        push_count: 0,
        startpos: expr.span().unstable().start().column
    };
    (folder.fold_expr(expr), folder.push_count > 0)
}
