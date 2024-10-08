let minus_operator = function
  | Token.NumberLiteral number -> Token.NumberLiteral (-.number)
  | _ -> failwith "Cannot negate sign of non-number"

let is_truthy = function
  | Token.BoolLiteral b -> b
  | Token.Null -> false
  | _ -> true

let bang_operator = function b -> Token.BoolLiteral (is_truthy b)

let rec evaluate = function
  | Expression.LiteralExpr literal -> literal
  | Expression.GroupingExpr expr -> evaluate expr
  | Expression.UnaryExpr (operator, right) -> (
      let right = evaluate right in
      match operator.token with
      | Token.Minus -> minus_operator right
      | Token.Bang -> bang_operator right
      | _ -> failwith "Not implemented")
  | Expression.BinaryExpr (left, operator, right) -> (
      let left = evaluate left in
      let right = evaluate right in
      match (left, right) with
      | Token.NumberLiteral left, Token.NumberLiteral right -> (
          match operator.token with
          | Token.Plus -> Token.NumberLiteral (left +. right)
          | Token.Minus -> Token.NumberLiteral (left -. right)
          | Token.Star -> Token.NumberLiteral (left *. right)
          | Token.Slash -> Token.NumberLiteral (left /. right)
          | Token.Greater -> Token.BoolLiteral (left > right)
          | Token.GreaterEqual -> Token.BoolLiteral (left >= right)
          | Token.Less -> Token.BoolLiteral (left < right)
          | Token.LessEqual -> Token.BoolLiteral (left <= right)
          | Token.EqualEqual -> Token.BoolLiteral (left = right)
          | Token.BangEqual -> Token.BoolLiteral (left <> right)
          | _ -> failwith "Invalid operator for numbers")
      | Token.StringLiteral left, Token.StringLiteral right -> (
          match operator.token with
          | Token.Plus -> Token.StringLiteral (left ^ right)
          | Token.EqualEqual -> Token.BoolLiteral (left = right)
          | Token.BangEqual -> Token.BoolLiteral (left <> right)
          | _ -> failwith "Invalid operator for strings")
      | Token.BoolLiteral left, Token.BoolLiteral right -> (
          match operator.token with
          | Token.And -> Token.BoolLiteral (left && right)
          | Token.Or -> Token.BoolLiteral (left || right)
          | Token.EqualEqual -> Token.BoolLiteral (left = right)
          | Token.BangEqual -> Token.BoolLiteral (left <> right)
          | _ -> failwith "Invalid operator for booleans")
      | Token.Null, Token.Null -> (
          match operator.token with
          | Token.EqualEqual -> Token.BoolLiteral true
          | Token.BangEqual -> Token.BoolLiteral false
          | _ -> failwith "Invalid operator for nil")
      | Token.Null, _ | _, Token.Null -> (
          match operator.token with
          | Token.EqualEqual -> Token.BoolLiteral false
          | Token.BangEqual -> Token.BoolLiteral true
          | _ -> failwith "Invalid operator")
      | _ -> failwith "Not implemented")
