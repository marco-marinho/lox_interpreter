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
        advance state
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
                    let flag, state = match_token [ Token.LeftParen ] state in

                    if flag then
                        let expr, state = expression state in
                        let _, state = consume Token.RightParen state "Expected ')' after expression" in
                        (expr, state)
                    else
                        (* let _, t = state in
            List.iter (fun x -> print_string (Token.string_of_token x)) t; *)
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
    let expr, state = comparison state in

    let rec local_match prev_expr state =
        match match_token [ Token.BangEqual; Token.EqualEqual ] state with
        | true, state ->
            let operator = previous state in
            let right, state = comparison state in
            let expr = Expression.BinaryExpr(prev_expr, operator, right) in
            local_match expr state
        | false, state -> (prev_expr, state) in

    local_match expr state

and expression state = equality state

let parse tokens =
    let rec aux acc state =
        if is_at_end state then
            List.rev acc
        else
            let expr, state = expression state in aux (expr :: acc) state in

    let state = ([], tokens) in

    try
        aux [] state
    with Failure s ->
        printfn "%s" s
        []
