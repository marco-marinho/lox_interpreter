module Lox.Expression

type expression =
    | BinaryExpr of expression * Token.token * expression
    | GroupingExpr of expression
    | LiteralExpr of Token.literal
    | UnaryExpr of Token.token * expression

let rec string_of_expression =
    function
    | BinaryExpr(left, operator, right) ->
        Printf.sprintf
            "Binary(%s, %s, %s)"
            (string_of_expression left)
            (Token.string_of_token operator)
            (string_of_expression right)
    | GroupingExpr expr -> Printf.sprintf "Grouping(%s)" (string_of_expression expr)
    | LiteralExpr literal -> Printf.sprintf "Literal(%s)" (Token.string_of_literal literal)
    | UnaryExpr(operator, right) ->
        Printf.sprintf "Unary(%s, %s)" (Token.string_of_token operator) (string_of_expression right)
