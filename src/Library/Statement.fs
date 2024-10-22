module Lox.Statement

type statement =
    | Statement of Expression.expression
    | PrintStatement of Expression.expression

let string_of_statement =
    function
    | Statement expr -> Printf.sprintf "Statement(%s)" (Expression.string_of_expression expr)
    | PrintStatement expr -> Printf.sprintf "PrintStatement(%s)" (Expression.string_of_expression expr)
