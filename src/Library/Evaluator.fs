module Lox.Evaluator

let minus_operator =
    function
    | Token.NumberLiteral number -> Token.NumberLiteral(-number)
    | _ -> failwith "Cannot negate sign of non-number"

let is_truthy =
    function
    | Token.BoolLiteral b -> b
    | Token.Null -> false
    | _ -> true

let bang_operator =
    function
    | b -> Token.BoolLiteral(is_truthy b)

let rec evaluate_expression expr environment =
    match expr with
    | Expression.AsignExpr(token, expr) ->
        let value, environment = evaluate_expression expr environment
        let environment = Environment.assign environment token.lexeme value
        (value, environment)
    | Expression.VariableExpr token -> (Environment.get environment token.lexeme, environment)
    | Expression.LiteralExpr literal -> (literal, environment)
    | Expression.GroupingExpr expr -> evaluate_expression expr environment
    | Expression.UnaryExpr(operator, right) ->
        let right, _ = evaluate_expression right environment in

        match operator.token with
        | Token.Minus -> (minus_operator right, environment)
        | Token.Bang -> (bang_operator right, environment)
        | _ -> failwith "Not implemented"
    | Expression.BinaryExpr(left, operator, right) ->
        let left, _ = evaluate_expression left environment in
        let right, _ = evaluate_expression right environment in

        match (left, right) with
        | Token.NumberLiteral left, Token.NumberLiteral right ->
            match operator.token with
            | Token.Plus -> (Token.NumberLiteral(left + right), environment)
            | Token.Minus -> (Token.NumberLiteral(left - right), environment)
            | Token.Star -> (Token.NumberLiteral(left * right), environment)
            | Token.Slash -> (Token.NumberLiteral(left / right), environment)
            | Token.Greater -> (Token.BoolLiteral(left > right), environment)
            | Token.GreaterEqual -> (Token.BoolLiteral(left >= right), environment)
            | Token.Less -> (Token.BoolLiteral(left < right), environment)
            | Token.LessEqual -> (Token.BoolLiteral(left <= right), environment)
            | Token.EqualEqual -> (Token.BoolLiteral(left = right), environment)
            | Token.BangEqual -> (Token.BoolLiteral(left <> right), environment)
            | _ -> failwith "Invalid operator for numbers"
        | Token.StringLiteral left, Token.StringLiteral right ->
            match operator.token with
            | Token.Plus -> (Token.StringLiteral(left + right), environment)
            | Token.EqualEqual -> (Token.BoolLiteral(left = right), environment)
            | Token.BangEqual -> (Token.BoolLiteral(left <> right), environment)
            | _ -> failwith "Invalid operator for strings"
        | Token.BoolLiteral left, Token.BoolLiteral right ->
            match operator.token with
            | Token.And -> (Token.BoolLiteral(left && right), environment)
            | Token.Or -> (Token.BoolLiteral(left || right), environment)
            | Token.EqualEqual -> (Token.BoolLiteral(left = right), environment)
            | Token.BangEqual -> (Token.BoolLiteral(left <> right), environment)
            | _ -> failwith "Invalid operator for booleans"
        | Token.Null, Token.Null ->
            match operator.token with
            | Token.EqualEqual -> (Token.BoolLiteral true, environment)
            | Token.BangEqual -> (Token.BoolLiteral false, environment)
            | _ -> failwith "Invalid operator for nil"
        | Token.Null, _
        | _, Token.Null ->
            match operator.token with
            | Token.EqualEqual -> (Token.BoolLiteral false, environment)
            | Token.BangEqual -> (Token.BoolLiteral true, environment)
            | _ -> failwith "Invalid operator"
        | _ -> failwith "Not implemented"


let evaluate_var_stmt name expr environment =
    let value, environment = evaluate_expression expr environment
    Environment.define environment name value


let evaluate_stament statement environment =
    match statement with
    | Statement.Statement expr ->
        evaluate_expression expr |> ignore
        environment
    | Statement.PrintStatement expr ->
        printfn "%s" (string (evaluate_expression expr environment))
        environment
    | Statement.VarStatement(name, expr) -> evaluate_var_stmt name.lexeme expr environment
