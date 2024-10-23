module Lox.Statement

type statement =
    | Statement of Expression.expression
    | PrintStatement of Expression.expression
    | VarStatement of Token.token * Expression.expression

    override this.ToString() =
        match this with
        | Statement expr -> sprintf "Statement(%s)" (string expr)
        | PrintStatement expr -> sprintf "PrintStatement(%s)" (string expr)
        | VarStatement(token, expr) -> sprintf "VarStatement(%s, %s)" (string token) (string expr)
