type state_type = Token.token list * Token.token list

let check token_type state =
  match state with
  | _, [] -> false
  | _, h :: _ -> if Token.token_type h = token_type then true else false

let is_at_end state =
  match state with _, x :: _ -> Token.token_type x = EOF | _ -> false

let peek state =
  match state with _, x :: _ -> x | _ -> failwith "No more tokens to peek"

let previous state =
  match state with h :: _, _ -> h | _ -> failwith "No previous token"

let advance state =
  if not (is_at_end state) then
    match state with
    | p, h :: t ->
        let next_state = (h :: p, t) in
        (previous next_state, next_state)
    | _ -> failwith "No more tokens to advance"
  else (previous state, state)

let match_token alternatives state =
  let rec aux state = function
    | [] -> (false, state)
    | h :: t ->
        if check h state then
          let _, next_state = advance state in
          (true, next_state)
        else aux state t
  in
  aux state alternatives

let rec primary state =
  let flag, state = match_token [ False ] state in
  if flag then (Expression.LiteralExpr (Token.BoolLiteral false), state)
  else
    let flag, state = match_token [ True ] state in
    if flag then (Expression.LiteralExpr (Token.BoolLiteral true), state)
    else
      let flag, state = match_token [ Nil ] state in
      if flag then (Expression.LiteralExpr Token.Null, state)
      else
        let flag, state = match_token [ Number; String ] state in
        if flag then
          (Expression.LiteralExpr (Token.token_literal (previous state)), state)
        else
          let flag, state = match_token [ LeftParen ] state in
          if flag then
            let expr, state = expression state in
            let flag, state = match_token [ RightParen ] state in
            if flag then (Expression.GroupingExpr expr, state)
            else failwith "Expect ')' after expression"
          else failwith "Expect primary"

and unary state =
  match match_token [ Bang; Minus ] state with
  | true, state ->
      let operator = previous state in
      let right, state = unary state in
      Expression.UnaryExpr (operator, right), state
  | false, state -> primary state

and factor state =
  let expr, state = unary state in
  let rec local_match prev_expr state =
    match match_token [ Slash; Star ] state with
    | true, state ->
        let operator = previous state in
        let right, state = unary state in
        let expr = Expression.BinaryExpr (prev_expr, operator, right) in
        local_match expr state
    | false, state -> (prev_expr, state)
  in
  local_match expr state

and term state =
  let expr, state = factor state in
  let rec local_match prev_expr state =
    match match_token [ Minus; Plus ] state with
    | true, state ->
        let operator = previous state in
        let right, state = factor state in
        let expr = Expression.BinaryExpr (prev_expr, operator, right) in
        local_match expr state
    | false, state -> (prev_expr, state)
  in
  local_match expr state

and comparison state =
  let expr, state = term state in
  let rec local_match prev_expr state =
    match match_token [ Greater; GreaterEqual; Less; LessEqual ] state with
    | true, state ->
        let operator = previous state in
        let right, state = term state in
        let expr = Expression.BinaryExpr (prev_expr, operator, right) in
        local_match expr state
    | false, state -> (prev_expr, state)
  in
  local_match expr state

and equality state =
  let expr, state = comparison state in
  let rec local_match prev_expr state =
    match match_token [ BangEqual; EqualEqual ] state with
    | true, state ->
        let operator = previous state in
        let right, state = comparison state in
        let expr = Expression.BinaryExpr (prev_expr, operator, right) in
        local_match expr state
    | false, state -> (prev_expr, state)
  in
  local_match expr state

and expression state = equality state
