module Lox.Evaluator

exception ReturnException of Environment.value * Map<string, Environment.value> list

let minus_operator =
    function
    | Environment.Value(Token.NumberLiteral number) -> Environment.Value(Token.NumberLiteral(-number))
    | _ -> failwith "Cannot negate sign of non-number"

let is_truthy =
    function
    | Environment.Value(Token.BoolLiteral b) -> Environment.Value(Token.BoolLiteral(b))
    | Environment.Value(Token.Null) -> Environment.Value(Token.BoolLiteral(false))
    | Environment.Value(Token.NumberLiteral n) -> Environment.Value(Token.BoolLiteral(n <> 0.0))
    | _ -> failwith "Cannot determine truthiness of non-boolean, non-null, non-number"

let bang_operator =
    function
    | Environment.Value(_) as v ->
        let truthy = is_truthy v

        match truthy with
        | Environment.Value(Token.BoolLiteral b) -> Environment.Value(Token.BoolLiteral(not b))
        | _ -> failwith "Cannot negate non-boolean"
    | _ -> failwith "Cannot negate non-literal"

let rec evaluate_expression expr environment =
    match expr with
    | Expression.Call(calle, _, arguments) ->
        let arguments, environment =
            List.fold
                (fun (acc, env) arg ->
                    let value, env = evaluate_expression arg env
                    value :: acc, env)
                ([], environment)
                arguments

        let callable, environment = evaluate_expression calle environment

        match callable with
        | Environment.Fun(Callable.Function(name, _, _) as func) ->
            let value, env = call environment func arguments
            value, env
        | _ -> failwith "Not a function"
    | Expression.LogicalExpr(left, operator, right) ->
        let left, environment = evaluate_expression left environment

        match is_truthy left with
        | Environment.Value(Token.BoolLiteral b) ->
            if operator.token = Token.Or && b then
                (left, environment)
            else if operator.token = Token.And && not b then
                (left, environment)
            else
                evaluate_expression right environment
        | _ -> failwith "Invalid logical operator"
    | Expression.AsignExpr(token, expr) ->
        let value, environment = evaluate_expression expr environment
        let environment = Environment.assign environment token.lexeme value
        (value, environment)
    | Expression.VariableExpr token -> (Environment.get environment token.lexeme, environment)
    | Expression.LiteralExpr literal -> (Environment.Value(literal), environment)
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
        | Environment.Value(Token.NumberLiteral left), Environment.Value(Token.NumberLiteral right) ->
            match operator.token with
            | Token.Plus -> (Environment.Value(Token.NumberLiteral(left + right)), environment)
            | Token.Minus -> (Environment.Value(Token.NumberLiteral(left - right)), environment)
            | Token.Star -> (Environment.Value(Token.NumberLiteral(left * right)), environment)
            | Token.Slash -> (Environment.Value(Token.NumberLiteral(left / right)), environment)
            | Token.Greater -> (Environment.Value(Token.BoolLiteral(left > right)), environment)
            | Token.GreaterEqual -> (Environment.Value(Token.BoolLiteral(left >= right)), environment)
            | Token.Less -> (Environment.Value(Token.BoolLiteral(left < right)), environment)
            | Token.LessEqual -> (Environment.Value(Token.BoolLiteral(left <= right)), environment)
            | Token.EqualEqual -> (Environment.Value(Token.BoolLiteral(left = right)), environment)
            | Token.BangEqual -> (Environment.Value(Token.BoolLiteral(left <> right)), environment)
            | _ -> failwith "Invalid operator for numbers"
        | Environment.Value(Token.StringLiteral left), Environment.Value(Token.StringLiteral right) ->
            match operator.token with
            | Token.Plus -> (Environment.Value(Token.StringLiteral(left + right)), environment)
            | Token.EqualEqual -> (Environment.Value(Token.BoolLiteral(left = right)), environment)
            | Token.BangEqual -> (Environment.Value(Token.BoolLiteral(left <> right)), environment)
            | _ -> failwith "Invalid operator for strings"
        | Environment.Value(Token.BoolLiteral left), Environment.Value(Token.BoolLiteral right) ->
            match operator.token with
            | Token.And -> (Environment.Value(Token.BoolLiteral(left && right)), environment)
            | Token.Or -> (Environment.Value(Token.BoolLiteral(left || right)), environment)
            | Token.EqualEqual -> (Environment.Value(Token.BoolLiteral(left = right)), environment)
            | Token.BangEqual -> (Environment.Value(Token.BoolLiteral(left <> right)), environment)
            | _ -> failwith "Invalid operator for booleans"
        | Environment.Value(Token.Null), Environment.Value(Token.Null) ->
            match operator.token with
            | Token.EqualEqual -> (Environment.Value(Token.BoolLiteral true), environment)
            | Token.BangEqual -> (Environment.Value(Token.BoolLiteral false), environment)
            | _ -> failwith "Invalid operator for nil"
        | Environment.Value(Token.Null), _
        | _, Environment.Value(Token.Null) ->
            match operator.token with
            | Token.EqualEqual -> (Environment.Value(Token.BoolLiteral false), environment)
            | Token.BangEqual -> (Environment.Value(Token.BoolLiteral true), environment)
            | _ -> failwith "Invalid operator"
        | _ -> failwith "Not implemented"


and evaluate_var_stmt name expr environment =
    let value, environment = evaluate_expression expr environment
    Environment.define environment name value


and evaluate_stament statement environment =
    match statement with
    | Statement.FunctionStatement(name, args, body) ->
        let func = Environment.Fun(Callable.Function(name.lexeme, args, body))
        let environment = Environment.define environment name.lexeme func
        environment
    | Statement.IfStatement(condition, then_branch, else_branch) ->
        let condition, _ = evaluate_expression condition environment

        match condition with
        | Environment.Value(Token.BoolLiteral b) ->
            if b then
                evaluate_stament then_branch environment
            else
                evaluate_stament else_branch environment
        | _ -> failwith "Invalid if condition"
    | Statement.Statement expr ->
        let _, environment = evaluate_expression expr environment
        environment
    | Statement.PrintStatement expr ->
        let res, environment = evaluate_expression expr environment

        match res with
        | Environment.Value(v) -> printfn "%s" (string v)
        | Environment.Fun(Callable.Function(name, _, _)) -> printfn "<function %s>" name
        | _ -> failwith "Invalid print"

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

            match condition with
            | Environment.Value(Token.BoolLiteral b) ->
                if b then
                    let env = evaluate_stament body env
                    loop env
                else
                    env
            | _ -> failwith "Invalid while condition"

        loop environment
    | Statement.ReturnStatement(keyword, value) ->
        let value, environment = evaluate_expression value environment
        raise (ReturnException(value, environment))

and call environment func arguments =
    match func with
    | Callable.Function(name, parameters, body) ->
        let environment =
            List.fold2
                (fun env (token: Token.token) value -> Environment.define env token.lexeme value)
                (Map.empty :: environment)
                parameters
                arguments

        let ret, temp_env =
            try
                let temp_env = evaluate_stament body environment
                Environment.Value(Token.Null), temp_env
            with ReturnException(value, temp_env) ->
                value, temp_env

        match temp_env with
        | _ :: tail -> ret, tail
        | [] -> failwith "Empty environment"
    | _ -> failwith "Not a function"
