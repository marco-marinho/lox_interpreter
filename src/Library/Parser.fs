module Lox.Parser

let check token_type state =
    match state with
    | _, [] -> false
    | _, h :: _ -> if Token.token_type h = token_type then true else false

let is_at_end state =
    match state with
    | _, x :: _ -> Token.token_type x = Token.EOF
    | _ -> false

let peek state =
    match state with
    | _, x :: _ -> x
    | _ -> failwith "No more tokens to peek"

let previous state =
    match state with
    | h :: _, _ -> h
    | _ -> failwith "No previous token"

let advance state =
    if not (is_at_end state) then
        match state with
        | p, h :: t -> let next_state = (h :: p, t) in (previous next_state, next_state)
        | _ -> failwith "No more tokens to advance"
    else
        failwith "No more tokens to advance"
        (previous state, state)

let drop_one state =
    let _, state = advance state
    state

let consume token_type state message =
    if check token_type state then
        let token, state = advance state
        token, state
    else
        failwith message

let match_token alternatives state =
    let rec aux state =
        function
        | [] -> (false, state)
        | h :: t ->
            if check h state then
                let _, next_state = advance state in (true, next_state)
            else
                aux state t in

    aux state alternatives

let synchronize state =
    let rec aux state =
        match state with
        | _, [] -> state
        | _, _ :: _ ->
            if Token.token_type (previous state) = Token.Semicolon then
                state
            else if
                List.fold
                    (fun acc x -> let y = Token.token_type (peek state) in y = x || acc)
                    true
                    [ Token.Class
                      Token.Fun
                      Token.Var
                      Token.For
                      Token.If
                      Token.While
                      Token.Print
                      Token.Return ]
            then
                let _, state = advance state in aux state
            else
                state in

    aux state

let rec primary state =
    match Token.token_type (peek state) with
    | Token.False ->
        let state = drop_one state
        (Expression.LiteralExpr(Token.BoolLiteral false), state)
    | Token.True ->
        let state = drop_one state
        (Expression.LiteralExpr(Token.BoolLiteral true), state)
    | Token.Nil ->
        let state = drop_one state
        (Expression.LiteralExpr Token.Null, state)
    | Token.Number
    | Token.String ->
        let _, state = match_token [ Token.Number; Token.String ] state
        (Expression.LiteralExpr(Token.token_literal (previous state)), state)
    | Token.Indentifier ->
        let state = drop_one state
        (Expression.VariableExpr(previous state), state)
    | Token.LeftParen ->
        let state = drop_one state
        let expr, state = expression state
        let _, state = consume Token.RightParen state "Expected ')' after expression"
        (expr, state)
    | _ -> failwith "Expected expression"

and unary state =
    match match_token [ Token.Bang; Token.Minus ] state with
    | true, state ->
        let operator = previous state
        let right, state = unary state
        (Expression.UnaryExpr(operator, right), state)
    | false, state -> primary state

and factor state =
    let expr, state = unary state

    let rec local_match prev_expr state =
        match match_token [ Token.Slash; Token.Star ] state with
        | true, state ->
            let operator = previous state
            let right, state = unary state
            let expr = Expression.BinaryExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and term state =
    let expr, state = factor state

    let rec local_match prev_expr state =
        match match_token [ Token.Minus; Token.Plus ] state with
        | true, state ->
            let operator = previous state
            let right, state = factor state
            let expr = Expression.BinaryExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and comparison state =
    let expr, state = term state

    let rec local_match prev_expr state =
        match match_token [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ] state with
        | true, state ->
            let operator = previous state
            let right, state = term state
            let expr = Expression.BinaryExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and equality state =
    let expr, state = comparison state

    let rec local_match prev_expr state =
        match match_token [ Token.BangEqual; Token.EqualEqual ] state with
        | true, state ->
            let operator = previous state
            let right, state = comparison state
            let expr = Expression.BinaryExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and and_expression state =
    let expr, state = equality state

    let rec local_match prev_expr state =
        match match_token [ Token.And ] state with
        | true, state ->
            let operator = previous state
            let right, state = equality state
            let expr = Expression.LogicalExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and or_expression state =
    let expr, state = and_expression state

    let rec local_match prev_expr state =
        match match_token [ Token.Or ] state with
        | true, state ->
            let operator = previous state
            let right, state = and_expression state
            let expr = Expression.LogicalExpr(prev_expr, operator, right)
            local_match expr state
        | false, state -> (prev_expr, state)

    local_match expr state

and assignment state =
    let expr, state = or_expression state

    match match_token [ Token.Equal ] state with
    | true, state ->
        let value, state = assignment state

        match expr with
        | Expression.VariableExpr(token) -> (Expression.AsignExpr(token, value), state)
        | _ -> failwith "Invalid assignment target"
    | false, state -> (expr, state)

and expression state = assignment state

let rec statement state =
    let _, next = state

    match Token.token_type (List.head next) with
    | Token.Print -> print_statement state
    | Token.LeftBrace -> block_statement state
    | Token.If -> if_statement state
    | Token.While -> while_statement state
    | Token.For -> for_statement state
    | _ -> expression_statement state

and for_statement state =
    let state = drop_one state
    let _, state = consume Token.LeftParen state "Expected '(' after for"

    // Parse initializer
    let _, next = state

    let initializer, state =
        match Token.token_type (List.head next) with
        | Token.Semicolon ->
            let state = drop_one state
            None, state
        | Token.Var ->
            let state = drop_one state
            let expr, state = var_declaration state
            Some(expr), state
        | _ ->
            let expr, state = expression_statement state
            Some(expr), state


    // Parse condition
    let _, next = state

    let condition, state =
        match Token.token_type (List.head next) with
        | Token.Semicolon ->
            let state = drop_one state
            Expression.LiteralExpr(Token.BoolLiteral true), state
        | _ -> expression state

    let _, state = consume Token.Semicolon state "Expected ';' after loop condition"

    // Parse increment
    let increment, state =
        match Token.token_type (List.head next) with
        | Token.RightParen -> None, state
        | _ ->
            let expr, state = expression state
            Some(expr), state

    let _, state = consume Token.RightParen state "Expected ')' after for clauses"

    // Parse body
    let body, state = statement state

    // Transform for statement into while statement, check if there is increment first
    let body =
        match increment with
        | Some(expr) -> Statement.BlockStatement([ body; Statement.Statement(expr) ])
        | None -> body

    // While statement with condition before body as usual
    let body = Statement.WhileStatement(condition, body)

    // If there is an initializer, run it before the while loop
    let body =
        match initializer with
        | Some(expr) -> Statement.BlockStatement([ expr; body ])
        | None -> body

    body, state


and while_statement state =
    let state = drop_one state
    let _, state = consume Token.LeftParen state "Expected '(' after while"
    let condition, state = expression state
    let _, state = consume Token.RightParen state "Expected ')' after while condition"
    let body, state = statement state
    Statement.WhileStatement(condition, body), state

and block_statement state =
    let state = drop_one state

    let rec aux state acc =
        if is_at_end state || check Token.RightBrace state then
            let _, state = consume Token.RightBrace state "Expected '}' after block"
            (Statement.BlockStatement(List.rev acc), state)
        else
            let stmt, state = declaration state
            aux state (stmt :: acc)

    aux state []

and var_declaration state =
    let token, state = consume Token.Indentifier state "Expected identifier after var"
    let has_value, state = match_token [ Token.Equal ] state

    let value, state =
        if has_value then
            expression state
        else
            Expression.LiteralExpr(Token.Null), state


    let _, state = consume Token.Semicolon state "Expected ; after var declaration"
    Statement.VarStatement(token, value), state

and declaration state =
    let is_var, state = match_token [ Token.Var ] state

    if is_var then var_declaration state else statement state

and if_statement state =
    let state = drop_one state
    let _, state = consume Token.LeftParen state "Expected '(' after if"
    let condition, state = expression state
    let _, state = consume Token.RightParen state "Expected ')' after if condition"
    let then_branch, state = statement state

    let has_else, state = match_token [ Token.Else ] state

    if has_else then
        let else_branch, state = statement state
        Statement.IfStatement(condition, then_branch, else_branch), state
    else
        Statement.IfStatement(condition, then_branch, Statement.BlockStatement []), state



and print_statement state =
    let state = drop_one state
    let value, state = expression state
    let _, state = consume Token.Semicolon state "Expected ; after value"
    Statement.PrintStatement(value), state

and expression_statement state =
    let value, state = expression state
    let _, state = consume Token.Semicolon state "Expected ; after value"
    Statement.Statement(value), state

let parse tokens =
    let rec aux acc state =

        if is_at_end state then
            List.rev acc
        else
            let stmt, state = declaration state in aux (stmt :: acc) state

    let state = ([], tokens)

    try
        aux [] state
    with Failure s ->
        printfn "%s" s
        []
