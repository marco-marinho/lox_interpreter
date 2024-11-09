module Lox.Statement

type statement =
    | Statement of Expression.expression
    | PrintStatement of Expression.expression
    | VarStatement of Token.token * Expression.expression
    | BlockStatement of statement list
    | IfStatement of Expression.expression * statement * statement
    | WhileStatement of Expression.expression * statement

    override this.ToString() =
        match this with
        | Statement expr -> sprintf "Statement(%s)" (string expr)
        | PrintStatement expr -> sprintf "PrintStatement(%s)" (string expr)
        | VarStatement(token, expr) -> sprintf "VarStatement(%s, %s)" (string token) (string expr)
        | BlockStatement statements -> sprintf "BlockStatement(%s)" (string statements)
        | IfStatement(condition, then_branch, else_branch) ->
            sprintf
                "IfStatement(condition: %s, then: %s, else: %s)"
                (string condition)
                (string then_branch)
                (string else_branch)
        | WhileStatement(condition, body) -> sprintf "WhileStatement(%s, %s)" (string condition) (string body)
