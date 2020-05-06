use crate::lexer::Token;
use crate::parser::{
    Type,
    Expr,
    Stmt,
    Fn,
    Prog
};

struct Scope<'a> {
    space: Vec<(String, Type)>,
    parent: Option<&'a Scope<'a>>,
}

fn scope_get<'a>(scope: &'a Scope, name: &str) -> Option<&'a Type> {
    for (n, t) in &scope.space {
        if n == name {
            return Some(t);
        }
    }

    if let Some(parent) = scope.parent {
        scope_get(parent, name)
    } else {
        None
    }
}

fn typecheck_expr_item(scope: &mut Scope,
    token: &Token)
    -> Type
{
    match token {
        Token::Symbol(name) => {
            if let Some(t) = scope_get(scope, name) {
                t.clone()
            } else {
                panic!("'{}' not in scope", name)
            }
        },
        Token::Bool(_) => Type::Bool,
        Token::Float(_) => Type::F64,
        Token::Integer(_) => Type::I64,
        Token::String(_) => Type::Array(Box::new(Type::U8)),
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
        Token::CompLess => {
            if a == Type::I64 && b == Type::I64 {
                Type::Bool
            } else {
                panic!("op '{}' cannot handle type {} / {}", token, a, b)
            }
        },

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

fn typecheck_expr(scope: &mut Scope,
    e: &Expr)
    -> (Type, Expr)
{
    match e {
        Expr::TempStart(_, _) | Expr::TempOp(_, _, _) => {
            panic!("malformed expr")
        },
        
        Expr::Op(token, a, b) => {
            let (a_t, na) = typecheck_expr(scope, a);
            let (b_t, nb) = typecheck_expr(scope, b);
            let t = typecheck_expr_op(token, a_t, b_t);

            (t, Expr::Op(token.clone(), Box::new(na), Box::new(nb)))
        },

        Expr::Call(name, args) => {
            let mut given_types = vec![];
            let mut n_args = vec![];

            for a in args {
                let (t, aa) = typecheck_expr(scope, a);
                n_args.push(aa);
                given_types.push(t);
            }

            let t = match scope_get(scope, name) {
                Some(Type::Fn(ret, expect_types)) => {
                    if args.len() != given_types.len() {
                        panic!("function '{}' expected {} args not {}",
                            name, expect_types.len(), given_types.len());
                    }

                    for (e, g) in expect_types.iter().zip(given_types.iter()) {
                        if e != g {
                            panic!("function '{}' expects arg type {} got {}",
                                name, e, g);
                        }
                    }

                    (**ret).clone()
                },
                Some(t) => {
                    panic!("'{}' with type {} not a function", name, t)
                },
                None => {
                    panic!("'{}' not in scope", name)
                },
            };

            (t, Expr::Call(name.clone(), n_args))
        },

        Expr::Item(token) => {
            let t = typecheck_expr_item(scope, token);
            (t, Expr::Item(token.clone()))
        },
    }
}

fn typecheck_stmt_alloc(scope: &mut Scope,
    name: &String,
    var_type: &Type,
    value: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (t, exp) = typecheck_expr(scope, value);

    if t != *var_type {
        panic!("alloc type mismatch expected {} != got {}",
            var_type, t);
    }

    scope.space.push((name.to_string(), t.clone()));

    (Type::None, Stmt::Alloc(name.clone(), t, Box::new(exp)))
}

fn typecheck_stmt_assign(scope: &mut Scope,
    name: &String,
    value: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (_t, exp) = typecheck_expr(scope, value);

    // TODO: check t against stored var type

    (Type::None, Stmt::Assign(name.clone(), Box::new(exp)))
}

fn typecheck_stmt_expr(scope: &mut Scope,
    e: &Box<Expr>) 
    -> (Type, Stmt)
{
    let (_t, ne) = typecheck_expr(scope, e);
    
    (Type::None, Stmt::Expr(Box::new(ne)))
}

fn typecheck_stmt_return(scope: &mut Scope,
    oe: &Option<Box<Expr>>) 
    -> (Type, Stmt)
{
    if let Some(e) = oe {
        let (t, ne) = typecheck_expr(scope, e);

        (t, Stmt::Return(Some(Box::new(ne))))
    } else {
        (Type::None, Stmt::Return(None))
    }
}

fn typecheck_stmt_if(scope: &mut Scope,
    cond: &Box<Expr>,
    then: &Box<Stmt>,
    otherwise: &Option<Box<Stmt>>) 
    -> (Type, Stmt)
{
    let (cond_type, cond_expr) = typecheck_expr(scope, cond);

    if cond_type != Type::Bool {
        panic!("cond bad type {}", cond_type);
    }

    let (then_type, then_stmt) = typecheck_stmt(scope, then);

    let other_stmt = if let Some(s) = otherwise {
        let (other_type, other_stmt) = typecheck_stmt(scope, s);

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

fn typecheck_stmt_list(scope: &mut Scope,
    list: &Vec<Stmt>) 
    -> (Type, Stmt)
{
    let mut ret = Type::None;
    let mut stmts = vec![];
    let last = list.len() - 1;

    /* TODO: sub scope scope */

    for (i, s) in list.iter().enumerate() {
        let (t, n) = typecheck_stmt(scope, s);
        if i == last {
            ret = t;

        } else if ret != Type::None {
            panic!("early return?");
        }

        stmts.push(n);
    }

    (ret, Stmt::List(stmts))
}

fn typecheck_stmt(scope: &mut Scope, s: &Stmt) -> (Type, Stmt)
{
	match s {
		Stmt::Return(expr) =>
			typecheck_stmt_return(scope, expr),
		Stmt::Alloc(name, var_type, value) =>
			typecheck_stmt_alloc(scope, name, var_type, value),
		Stmt::Assign(name, value) =>
			typecheck_stmt_assign(scope, name, value),
		Stmt::If(cond, then, otherwise) =>
			typecheck_stmt_if(scope, cond, then, otherwise),
		Stmt::List(l) => 
			typecheck_stmt_list(scope, l),
		Stmt::Expr(e) =>
			typecheck_stmt_expr(scope, e),
	}
}

fn typecheck_func(parent_scope: &Scope, f: &Fn) -> Fn {

    let params = f.params.to_vec();

    let mut scope = Scope {
        space: f.params.to_vec(),
        parent: Some(parent_scope),
    };

    let (ret, stmts) = typecheck_stmt(&mut scope, &f.stmts);

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

    let mut scope = Scope {
        space: vec![],
        parent: None,
    };

    scope.space.push(("print".to_string(), Type::Fn(Box::new(Type::None), vec![Type::I64])));

    for (n, f) in &p.funcs {
        let mut arg_types = vec![];
        for (_pn, pt) in &f.params {
            arg_types.push(pt.clone());
        }

        scope.space.push((n.to_string(), 
            Type::Fn(Box::new(f.ret.clone()), arg_types)));
    }

    for (n, f) in &p.funcs {
        v.push((n.clone(), typecheck_func(&scope, f)));
    }

    Prog {
        funcs: v,
    }
}

