use std::fmt;

use crate::lexer::Token;
use crate::parser::Type;
use crate::parser::Expr;
use crate::parser::Stmt;
use crate::parser::Fn;
use crate::parser::Prog;

fn typecheck_expr_item(vars: &mut Vec<(String, Type)>, 
    token: &Token)
    -> Type
{
    match token {
        Token::Symbol(name) => {
        panic!("token {} type hasn't been implimented", token)
        },
        Token::Integer(_) => Type::I64,
        Token::Bool(_) => Type::Bool,
        Token::Float(f) => panic!("token {} float type not implimented", f),
        Token::String(s) => panic!("token {} string type not implimented", s),
        _ => panic!("token {} doesn't have item type", token),
    }
}

fn typecheck_expr_op(token: &Token, a: Type, b: Type)
    -> Type
{
    match token {
        Token::CompEqual | 
        Token::CompInEqual | 
        Token::CompGreaterEqual | 
        Token::CompGreater |
        Token::CompLessEqual |
        Token::CompLess |
        Token::OpMul | 
        Token::OpDiv | 
        Token::OpAdd | 
        Token::OpSub => {
            if a == Type::I64 && b == Type::I64 {
                Type::I64
            } else {
                panic!("op '{}' cannot handle type {} / {}", token, a, b)
            }
        },

        Token::OpRem => {
            if a == Type::I64 && b == Type::I64 {
                Type::I64
            } else {
                panic!("op '{}' cannot handle type {} / {}", token, a, b)
            }
        },

        Token::CompAnd |
        Token::CompOr => {
            if a == Type::Bool && b == Type::Bool {
                Type::Bool
            } else {
                panic!("op '{}' cannot handle type {} / {}", token, a, b)
            }
        },

        _ => panic!("token {} shouldn't be in expr op", token),
    }
}

fn typecheck_expr(vars: &mut Vec<(String, Type)>, 
    e: &Expr)
    -> (Type, Expr)
{
    match e {
        Expr::TempStart(_, _) | Expr::TempOp(_, _, _) => {
            panic!("malformed expr")
        },
        
        Expr::Op(token, a, b) => {
            let (a_t, na) = typecheck_expr(vars, a);
            let (b_t, nb) = typecheck_expr(vars, b);
            let t = typecheck_expr_op(token, a_t, b_t);

            (t, Expr::Op(token.clone(), Box::new(na), Box::new(nb)))
        },

        Expr::Call(name, args) => {
            let mut n_args = vec![];

            // TODO: pull up func

            for a in args {
                // TODO: check args */
                let (t, aa) = typecheck_expr(vars, a);
                n_args.push(aa);
            }

            (Type::None, Expr::Call(name.clone(), n_args))
        },

        Expr::Item(token) => {
            let t = typecheck_expr_item(vars, token);
            (t, Expr::Item(token.clone()))
        },
    }
}

fn typecheck_stmt_alloc(vars: &mut Vec<(String, Type)>, 
    name: &String,
    var_type: &Type,
    value: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (t, exp) = typecheck_expr(vars, value);

    if t != *var_type {
        panic!("alloc type mismatch expected {} != got {}",
            var_type, t);
    }

    (Type::None, Stmt::Alloc(name.clone(), t, Box::new(exp)))
}

fn typecheck_stmt_assign(vars: &mut Vec<(String, Type)>, 
    name: &String,
    value: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (_t, exp) = typecheck_expr(vars, value);

    // TODO: check t against stored var type

    (Type::None, Stmt::Assign(name.clone(), Box::new(exp)))
}

fn typecheck_stmt_expr(vars: &mut Vec<(String, Type)>, 
    e: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (_t, ne) = typecheck_expr(vars, e);
    
    (Type::None, Stmt::Expr(Box::new(ne)))
}

fn typecheck_stmt_return(vars: &mut Vec<(String, Type)>, 
    oe: &Option<Box<Expr>>) 
    -> (Type, Stmt)
{
    if let Some(e) = oe {
        let (t, ne) = typecheck_expr(vars, e);

        (t, Stmt::Return(Some(Box::new(ne))))
    } else {
        (Type::None, Stmt::Return(None))
    }
}

fn typecheck_stmt_if(vars: &mut Vec<(String, Type)>, 
    cond: &Box<Expr>,
    then: &Box<Stmt>,
    otherwise: &Option<Box<Stmt>>) 
    -> (Type, Stmt)
{
    let (cond_type, cond_expr) = typecheck_expr(vars, cond);

    if cond_type != Type::Bool {
        panic!("cond bad type {}", cond_type);
    }

    let (then_type, then_stmt) = typecheck_stmt(vars, then);

    let other_stmt = if let Some(s) = otherwise {
        let (other_type, other_stmt) = typecheck_stmt(vars, s);

        if other_type != then_type {
            panic!("branch type mismatch then {} != else {}",
                then_type, other_type);
        }

        Some(Box::new(other_stmt))
    } else {
        None
    };

    (then_type, 
        Stmt::If(Box::new(cond_expr), 
                Box::new(then_stmt), 
                other_stmt))
}

fn typecheck_stmt_list(vars: &mut Vec<(String, Type)>, 
    list: &Vec<Stmt>) 
    -> (Type, Stmt)
{
    let mut ret = Type::None;
    let mut stmts = vec![];
    let last = list.len() - 1;

    /* TODO: sub scope vars */

    for (i, s) in list.iter().enumerate() {
        let (t, n) = typecheck_stmt(vars, s);
        if i == last {
            ret = t;

        } else if ret != Type::None {
            panic!("early return?");
        }

        stmts.push(n);
    }

    (ret, Stmt::List(stmts))
}

fn typecheck_stmt(vars: &mut Vec<(String, Type)>, s: &Stmt) -> (Type, Stmt)
{
	match s {
		Stmt::Return(expr) =>
			typecheck_stmt_return(vars, expr),
		Stmt::Alloc(name, var_type, value) =>
			typecheck_stmt_alloc(vars, name, var_type, value),
		Stmt::Assign(name, value) =>
			typecheck_stmt_assign(vars, name, value),
		Stmt::If(cond, then, otherwise) =>
			typecheck_stmt_if(vars, cond, then, otherwise),
		Stmt::List(l) => 
			typecheck_stmt_list(vars, l),
		Stmt::Expr(e) =>
			typecheck_stmt_expr(vars, e),
	}
}

fn typecheck_func(f: Fn) -> Fn {

    let mut vars = f.params.to_vec();
    let params = f.params.to_vec();

    let (ret, stmts) = typecheck_stmt(&mut vars, &f.stmts);

    if ret != f.ret {
        panic!("function return type {} doesn't match {}", ret, f.ret);
    }
    
    Fn {
        params: params,
        ret: ret,
        stmts: stmts,
    }
}

pub fn typecheck(p: Prog) -> Prog {
    let mut v = vec![];

    for (n, f) in p.funcs {
        v.push((n.clone(), typecheck_func(f)));
    }

    Prog {
        funcs: v,
    }
}

