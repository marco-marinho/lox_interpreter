module Lox.Expression

type expression =
    | BinaryExpr of expression * Token.token * expression
    | GroupingExpr of expression
    | LiteralExpr of Token.literal
    | UnaryExpr of Token.token * expression
    | VariableExpr of Token.token
    | AsignExpr of Token.token * expression
    | LogicalExpr of expression * Token.token * expression
    | Call of expression * Token.token * expression list

    override this.ToString() =
        match this with
        | BinaryExpr(left, operator, right) ->
            Printf.sprintf "Binary(%s, %s, %s)" (string left) (string operator) (string right)
        | GroupingExpr expr -> Printf.sprintf "Grouping(%s)" (string expr)
        | LiteralExpr literal -> Printf.sprintf "Literal(%s)" (string literal)
        | UnaryExpr(operator, right) -> Printf.sprintf "Unary(%s, %s)" (string operator) (string right)
        | VariableExpr token -> Printf.sprintf "Variable(%s)" (string token)
        | AsignExpr(token, expr) -> Printf.sprintf "Asignement(%s, %s)" (string token) (string expr)
        | LogicalExpr(left, operator, right) ->
            Printf.sprintf "Logical(%s, %s, %s)" (string left) (string operator) (string right)
        | Call(callee, paren, arguments) ->
            Printf.sprintf "Call(%s, %s, %A)" (string callee) (string paren) (string arguments)
