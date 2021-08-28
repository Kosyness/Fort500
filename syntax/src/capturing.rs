/*
Basically

enum Stmt { 
    Declaration(varname, type)
}

enum Expr { 
    Stmt(Stmt)
}

Program(
    let program = (Program::MaybeDeclarations(), Program::Statements(), Program::MaybeSubprograms())

    where 
    match keyword => {
        One of the delcarations keywords => run_delcarations() Until unexpected token
        Else => error but since it's maybe declaratins -> ignore it
    }
)

*/

// use std::{borrow::BorrowMut, mem};

// use lexer::TokenSpan;

// use crate::context::Context;

// pub trait Tokens : Iterator { 
//     fn set_ctx(&mut self, ctx: Context);
//     fn ctx(&self) -> Context;
//     // fn token_context(&self) -> 
// }

// #[derive(Clone)]
// pub struct TokensInput { 
//     iter: <Vec<TokenSpan> as IntoIterator>::IntoIter,
//     ctx: Context,
// }

// impl Iterator for TokensInput { 
//     type Item = TokenSpan;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter.next()
//     }
// }

// impl Tokens for TokensInput { 
//     fn set_ctx(&mut self, ctx: Context) {
//         self.ctx = ctx;
//     }

//     fn ctx(&self) -> Context {
//         self.ctx
//     }
// }

// #[derive(Clone)]
// pub struct Capturing<I: Tokens> { 
//     inner: I,
//     captured: Vec<TokenSpan>
// }

// impl <I: Tokens> Capturing<I> { 
//     pub fn new(input: I) -> Self { 
//         Self { 
//             inner: input,
//             captured: Default::default()
//         }
//     }

//     pub fn take(&mut self) -> Vec<TokenSpan> { 
//         mem::replace(&mut *self.captured.borrow_mut(), Default::default())
//     }
// }

// impl <I: Tokens> Iterator for Capturing<I> { 
//     type Item = TokenSpan;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self.inner.next() { 
//             Some(ts) => {
//                 let mut v = self.captured;

//                 while let Some(last) = v.last() { 
//                     if last.span.lo >= ts.span.lo { 
//                         v.pop();
//                     } else {
//                         break;
//                     }
//                 }

//                 v.push(ts.clone());

//                 Some(ts)
//             }
//             None => None
//         }
//     }
// }