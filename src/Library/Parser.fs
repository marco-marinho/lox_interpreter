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
        (previous state, state)

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
    let flag, state = match_token [ Token.False ] state in

    if flag then
        (Expression.LiteralExpr(Token.BoolLiteral false), state)
    else
        let flag, state = match_token [ Token.True ] state in

        if flag then
            (Expression.LiteralExpr(Token.BoolLiteral true), state)
        else
            let flag, state = match_token [ Token.Nil ] state in

            if flag then
                (Expression.LiteralExpr Token.Null, state)
            else
                let flag, state = match_token [ Token.Number; Token.String ] state in

                if flag then
                    (Expression.LiteralExpr(Token.token_literal (previous state)), state)
                else
                    let flag, state = match_token [ Token.Indentifier ] state in

                    if flag then
                        (Expression.VariableExpr(previous state), state)
                    else
                        let flag, state = match_token [ Token.LeftParen ] state in

                        if flag then
                            let expr, state = expression state in
                            let _, state = consume Token.RightParen state "Expected ')' after expression" in
                            (expr, state)
                        else
                            failwith "Expected expression"

and unary state =
    match match_token [ Token.Bang; Token.Minus ] state with
    | true, state ->
        let operator = previous state in
        let right, state = unary state in
        (Expression.UnaryExpr(operator, right), state)
    | false, state -> primary state

and factor state =
    let expr, state = unary state in

    let rec local_match prev_expr state =
        match match_token [ Token.Slash; Token.Star ] state with
        | true, state ->
            let operator = previous state in
            let right, state = unary state in
            let expr = Expression.BinaryExpr(prev_expr, operator, right) in
            local_match expr state
        | false, state -> (prev_expr, state) in

    local_match expr state

and term state =
    let expr, state = factor state in

    let rec local_match prev_expr state =
        match match_token [ Token.Minus; Token.Plus ] state with
        | true, state ->
            let operator = previous state in
            let right, state = factor state in
            let expr = Expression.BinaryExpr(prev_expr, operator, right) in
            local_match expr state
        | false, state -> (prev_expr, state) in

    local_match expr state

and comparison state =
    let expr, state = term state in

    let rec local_match prev_expr state =
        match match_token [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ] state with
        | true, state ->
            let operator = previous state in
            let right, state = term state in
            let expr = Expression.BinaryExpr(prev_expr, operator, right) in
            local_match expr state
        | false, state -> (prev_expr, state) in

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

and assignment state =
    let expr, state = equality state

    match match_token [ Token.Equal ] state with
    | true, state ->
        let value, state = assignment state

        match expr with
        | Expression.VariableExpr(token) -> (Expression.AsignExpr(token, value), state)
        | _ -> failwith "Invalid assignment target"
    | false, state -> (expr, state)

and expression state = assignment state

let print_statement state =
    let value, state = expression state
    let _, state = consume Token.Semicolon state "Expected ; after value"
    Statement.PrintStatement(value), state

let expression_statement state =
    let value, state = expression state
    let _, state = consume Token.Semicolon state "Expected ; after value"
    Statement.Statement(value), state

let rec block_statement state =
    let rec aux state acc =
        let a, b = state

        if is_at_end state || check Token.RightBrace state then
            let _, state = consume Token.RightBrace state "Expected '}' after block"
            (Statement.BlockStatement(List.rev acc), state)
        else
            let stmt, state = declaration state
            aux state (stmt :: acc)

    aux state []


and statement state =
    let is_print, state = match_token [ Token.Print ] state

    if is_print then
        print_statement state
    else
        let is_block, state = match_token [ Token.LeftBrace ] state

        if is_block then
            block_statement state
        else
            expression_statement state

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
