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
    | Expression.Call(calle, _, arguments) ->
        let callee, environment = evaluate_expression calle environment

        let arguments, environment =
            List.fold
                (fun (acc, env) arg ->
                    let arg, env = evaluate_expression arg env
                    (arg :: acc, env))
                ([], environment)
                arguments

        failwith "Not implemented"
    | Expression.LogicalExpr(left, operator, right) ->
        let left, environment = evaluate_expression left environment

        if operator.token = Token.Or && is_truthy left then
            (left, environment)
        else if operator.token = Token.And && not (is_truthy left) then
            (left, environment)
        else
            evaluate_expression right environment
    | Expression.AsignExpr(token, expr) ->
        let value, environment = evaluate_expression expr environment
        let environment = Environment.assign environment token.lexeme value
        (value, environment)
    | Expression.VariableExpr token -> (Environment.get environment token.lexeme, environment)
    | Expression.LiteralExpr literal -> (literal, environment)
    | Expression.GroupingExpr expr -> evaluate_expression expr environment
    | Expression.UnaryExpr(operator, right) ->
        let right, _ = evaluate_expression right environment

        match operator.token with
        | Token.Minus -> (minus_operator right, environment)
        | Token.Bang -> (bang_operator right, environment)
        | _ -> failwith "Not implemented"
    | Expression.BinaryExpr(left, operator, right) ->
        let left, _ = evaluate_expression left environment
        let right, _ = evaluate_expression right environment

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


let rec evaluate_stament statement environment =
    match statement with
    | Statement.IfStatement(condition, then_branch, else_branch) ->
        let condition, _ = evaluate_expression condition environment

        if is_truthy condition then
            evaluate_stament then_branch environment
        else
            evaluate_stament else_branch environment
    | Statement.Statement expr ->
        let _, environment = evaluate_expression expr environment
        environment
    | Statement.PrintStatement expr ->
        let res, environment = evaluate_expression expr environment
        printfn "%s" (string res)
        environment
    | Statement.VarStatement(name, expr) -> evaluate_var_stmt name.lexeme expr environment
    | Statement.BlockStatement statements ->
        let temp_env =
            List.fold (fun env stmt -> evaluate_stament stmt env) (Map.empty :: environment) statements

        match temp_env with
        | _ :: tail -> tail
        | [] -> failwith "Empty environment"
    | Statement.WhileStatement(condition, body) ->
        let rec loop env =
            let condition, _ = evaluate_expression condition env

            if is_truthy condition then
                let env = evaluate_stament body env
                loop env
            else
                env

        loop environment
