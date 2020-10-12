use std::collections::HashMap;
use std::error;
use std::fmt;
use std::iter::Peekable;

#[derive(Debug)]
pub enum Expr {
    Value(f64),
    BinExpr(char, Box<Expr>, Box<Expr>),
    UnaryExpr(char, Box<Expr>),
    FuncCall(String, Vec<Expr>),
}

pub struct ParseContext<I: Iterator<Item=char>> {
    it: Peekable<I>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    Unexpected(String),
    Expecting(&'static str),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParseError is here!")
    }
}

impl error::Error for ParseError {}

impl<I> ParseContext<I>
    where
        I: Iterator<Item=char>
{
    fn new(i: I) -> Self {
        Self { it: i.peekable() }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.it.peek() {
            if c.is_whitespace() {
                self.it.next().unwrap();
            } else {
                break;
            }
        }
    }

    fn parse_additive_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_term()?;
        loop {
            self.skip_whitespace();

            match self.it.peek() {
                Some(&c @ '+') | Some(&c @ '-') => {
                    self.it.next().unwrap();
                    let rhs = self.parse_term()?;
                    lhs = Expr::BinExpr(c, Box::new(lhs), Box::new(rhs));
                }

                _ => {
                    return Ok(lhs);
                }
            }
        }
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;
        loop {
            self.skip_whitespace();

            match self.it.peek() {
                Some(&c @ '*') | Some(&c @ '/') => {
                    self.it.next().unwrap();
                    let rhs = self.parse_primary()?;
                    lhs = Expr::BinExpr(c, Box::new(lhs), Box::new(rhs));
                }

                _ => {
                    return Ok(lhs);
                }
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        self.skip_whitespace();

        match self.it.peek() {
            Some('(') => {
                self.it.next().unwrap();
                let ret = self.parse_additive_expr();
                match self.it.next() {
                    Some(')') => ret,
                    Some(_) => Err(ParseError::Expecting(")")),
                    None => Err(ParseError::UnexpectedEOF),
                }
            }
            Some('0'..='9') => self.parse_literal(),
            Some(&c @ '-') | Some(&c @ '+') => {
                self.it.next().unwrap();
                let operand = self.parse_primary()?;
                Ok(Expr::UnaryExpr(c, Box::new(operand)))
            }
            Some('a'..='z') | Some('A'..='Z') => self.parse_func_call(),
            Some(&c) => Err(ParseError::Unexpected(String::from(c))),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_func_call(&mut self) -> Result<Expr, ParseError> {
        let func_name = self.parse_func_name();
        self.skip_whitespace();
        if let Some('(') = self.it.next() {
            self.skip_whitespace();
            let mut args = Vec::new();
            match self.it.peek() {
                Some(')') => {
                    self.it.next().unwrap();
                    return Ok(Expr::FuncCall(func_name, args));
                }
                _ => loop {
                    let e = self.parse_additive_expr()?;
                    args.push(e);
                    self.skip_whitespace();
                    match self.it.next() {
                        Some(',') => {
                            continue;
                        }
                        Some(')') => {
                            return Ok(Expr::FuncCall(func_name, args));
                        }
                        Some(c) => {
                            return Err(ParseError::Unexpected(String::from(c)));
                        }
                        None => {
                            return Err(ParseError::UnexpectedEOF);
                        }
                    }
                }
            }
        }

        Err(ParseError::Expecting("("))
    }

    fn parse_func_name(&mut self) -> String {
        let mut s = String::new();
        while let Some(&c) = self.it.peek() {
            if c.is_alphanumeric() {
                self.it.next().unwrap();
                s.push(c);
            } else {
                break;
            }
        }
        s
    }

    fn parse_literal(&mut self) -> Result<Expr, ParseError> {
        self.skip_whitespace();
        let mut integral = 0.0;
        loop {
            match self.it.peek() {
                Some(&c @ '0'..='9') => {
                    self.it.next().unwrap();
                    integral = (c as u32 - '0' as u32) as f64 + integral * 10.0;
                }
                Some('.') => {
                    self.it.next().unwrap();
                    break;
                }
                _ => {
                    return Ok(Expr::Value(integral));
                }
            }
        }

        let mut decimal = 0.0;
        let mut decimal_precision = 0.1;
        loop {
            match self.it.peek() {
                Some(&c @ '0'..='9') => {
                    self.it.next().unwrap();
                    decimal = (c as u32 - '0' as u32) as f64 * decimal_precision + decimal;
                    decimal_precision /= 10.0;
                }
                _ => {
                    return Ok(Expr::Value(integral + decimal));
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    InternalError,
    UnknownFunction,
    InvalidNumberOfArguments,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EvalError is here!")
    }
}

impl error::Error for EvalError {}

pub struct EvalContext {
    functions: HashMap<String, Box<dyn Fn(&EvalContext, &Vec<Expr>) -> Result<f64, EvalError>>>,
}

impl EvalContext {
    fn add_function1(&mut self, name: String, func: impl Fn(f64) -> f64 + 'static) {
        self.functions.insert(
            name,
            Box::new(move |ctx, args: &Vec<Expr>| {
                if args.len() != 1 {
                    return Err(EvalError::InvalidNumberOfArguments);
                }
                args[0].eval(ctx).map(|x| func(x))
            }),
        );
    }

    fn add_function2(&mut self, name: String, func: impl Fn(f64, f64) -> f64 + 'static) {
        self.functions.insert(
            name,
            Box::new(move |ctx, args: &Vec<Expr>| {
                if args.len() != 2 {
                    return Err(EvalError::InvalidNumberOfArguments);
                }
                Ok(func(args[0].eval(ctx)?, args[1].eval(ctx)?))
            }),
        );
    }
}

macro_rules! add_function1 {
    ($ctx:expr, $($name:ident),*) => {{
        let ctx1: &mut EvalContext = $ctx;
        $(
            ctx1.add_function1(stringify!($name).into(), |x| x.$name());
        )*
    }};
}

trait Factorial {
    type Output;
    fn factorial(&self) -> Self::Output;
}

impl Factorial for f64 {
    type Output = f64;

    fn factorial(&self) -> Self::Output {
        let mut result = 1;
        for i in 1..=(*self as i32) {
            result *= i;
        }
        result as Self::Output
    }
}

impl Default for EvalContext {
    fn default() -> EvalContext {
        let mut ctx = EvalContext {
            functions: HashMap::new(),
        };
        {
            add_function1!(&mut ctx, sqrt, cbrt, exp, sin, cos, tan, asin, acos, atan, factorial);

            ctx.add_function2("log".into(), |x, y| {
                x.log(y)
            });

            ctx.add_function2("pow".into(), |x, y| {
                x.powf(y)
            })
        }
        ctx
    }
}

impl Expr {
    fn eval(&self, ctx: &EvalContext) -> Result<f64, EvalError> {
        match &self {
            Expr::Value(n) => Ok(*n),
            Expr::BinExpr('+', lhs, rhs) => Ok(lhs.eval(ctx)? + rhs.eval(ctx)?),
            Expr::BinExpr('-', lhs, rhs) => Ok(lhs.eval(ctx)? - rhs.eval(ctx)?),
            Expr::BinExpr('*', lhs, rhs) => Ok(lhs.eval(ctx)? * rhs.eval(ctx)?),
            Expr::BinExpr('/', lhs, rhs) => Ok(lhs.eval(ctx)? / rhs.eval(ctx)?),
            Expr::UnaryExpr('+', operand) => Ok(operand.eval(ctx)?),
            Expr::UnaryExpr('-', operand) => Ok(-operand.eval(ctx)?),
            Expr::FuncCall(func_name, args) => match ctx.functions.get(func_name) {
                Some(func) => func(ctx, args),
                None => Err(EvalError::UnknownFunction),
            },
            _ => Err(EvalError::InternalError),
        }
    }
}

/// Parse an expression into AST
pub fn parse_expr(expr: &str) -> Result<Expr, ParseError> {
    let mut ctx = ParseContext::new(expr.chars());
    let res = ctx.parse_additive_expr();
    ctx.skip_whitespace();
    match ctx.it.next() {
        Some(c) => Err(ParseError::Unexpected(String::from(c))),
        None => res,
    }
}

/// Evaluate the expression AST
pub fn eval_expr(expr: &Expr) -> Result<f64, EvalError> {
    expr.eval(&EvalContext::default())
}

#[derive(Debug)]
pub enum ParseOrEvalError {
    ParseError(ParseError),
    EvalError(EvalError)
}

impl fmt::Display for ParseOrEvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParseOrEvalError is here!")
    }
}

impl error::Error for ParseOrEvalError {

}

impl From<ParseError> for ParseOrEvalError {
    fn from(e: ParseError) -> ParseOrEvalError {
        ParseOrEvalError::ParseError(e)
    }
}

impl From<EvalError> for ParseOrEvalError {
    fn from(e: EvalError) -> ParseOrEvalError {
        ParseOrEvalError::EvalError(e)
    }
}

/// Parse and evaluate an expression
///
/// @param expr a string containing the expression
pub fn parse_and_eval_expr(expr: &str) -> Result<f64, ParseOrEvalError > {
    parse_expr(expr)
        .map_err(ParseOrEvalError::from)
        .and_then(|x| eval_expr(&x).map_err(ParseOrEvalError::from))
}

use std::ffi::CStr;
use std::os::raw::{c_char, c_double};

/// Parse and evaluate an expression
///
/// @param expr a nul-terminated string containing the expression
/// @param result pointer to a floating pointer number where the results will be stored after a successful call to this function
/// @return true if success; otherwise false
#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn eval_expr_c(expr: *const c_char, result: &mut c_double) -> bool {
    unsafe {
        CStr::from_ptr(expr)
    }
        .to_str()
        .map_or(None, Some)
        .and_then(|expr| parse_and_eval_expr(expr).map_or(None, Some))
        .map_or(false, |val| {
            *result = val as c_double;
            true
        })
}
