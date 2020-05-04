use std::fmt;

use crate::lexer::Token;
use crate::parser::Type;
use crate::parser::Expr;
use crate::parser::Stmt;
use crate::parser::Fn;
use crate::parser::Prog;

fn typecheck_stmt_alloc(vars: &mut Vec<(String, Type)>, 
    name: String,
    var_type: Type,
    value: Box<Expr>) 
    -> (Type, Stmt)
{
    (Type::None, Stmt::Return(None))
}

fn typecheck_stmt_assign(vars: &mut Vec<(String, Type)>, 
    name: String,
    value: Box<Expr>) 
    -> (Type, Stmt)
{
    (Type::None, Stmt::Return(None))
}

fn typecheck_stmt_expr(vars: &mut Vec<(String, Type)>, 
    e: Box<Expr>) 
    -> (Type, Stmt)
{
    (Type::None, Stmt::Return(None))
}

fn typecheck_stmt_return(vars: &mut Vec<(String, Type)>, 
    exp: Option<Box<Expr>>) 
    -> (Type, Stmt)
{
    (Type::Unknown, Stmt::Return(None))
}

fn typecheck_stmt_if(vars: &mut Vec<(String, Type)>, 
    cond: Box<Expr>,
    then: Box<Stmt>,
    otherwise: Option<Box<Stmt>>) 
    -> (Type, Stmt)
{
    (Type::Unknown, Stmt::Return(None))
}

fn typecheck_stmt_list(vars: &mut Vec<(String, Type)>, 
    list: Vec<Stmt>) 
    -> (Type, Stmt)
{
    let mut ret = Type::Unknown;
    let mut stmts = vec![];

    /* this is very wrong.
       I think I need to get rid of return and
       do something like rust. I prefer that anyway.
    */

    for s in list {
        let (t, n) = typecheck_stmt(vars, s);
        if t != Type::Unknown {
            if ret != Type::Unknown {
                if t != ret {
                    panic!("scope ret with different type {} from past {}",
                        t, ret);
                }
            }

            ret = t;
        }

        stmts.push(n);
    }

    (ret, Stmt::List(stmts))
}

fn typecheck_stmt(vars: &mut Vec<(String, Type)>, s: Stmt) -> (Type, Stmt)
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

    let (ret, stmts) = typecheck_stmt(&mut vars, f.stmts);

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

