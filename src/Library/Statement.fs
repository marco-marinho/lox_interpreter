module Lox.Statement

type statement =
    | Statement of Expression.expression
    | PrintStatement of Expression.expression
    | VarStatement of Token.token * Expression.expression
    | BlockStatement of statement list
    | IfStatement of Expression.expression * statement * statement
    | WhileStatement of Expression.expression * statement
    | FunctionStatement of Token.token * Token.token list * statement
    | ReturnStatement of Token.token * Expression.expression

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
        | FunctionStatement(name, parameters, body) ->
            sprintf "FunctionStatement(%s, %s, %s)" (string name) (string parameters) (string body)
        | ReturnStatement(keyword, value) -> sprintf "ReturnStatement(%s, %s)" (string keyword) (string value)
