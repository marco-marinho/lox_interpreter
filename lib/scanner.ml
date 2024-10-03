let scanner input =
  let rec aux acc idx line input =
    if idx = String.length input then List.rev acc
    else
      match input.[idx] with
      | '(' -> aux (Token.make_token Token.LeftParen "(" Token.Null line :: acc) (idx + 1) line input
      | ')' -> aux (Token.make_token Token.RightParen ")" Token.Null line :: acc) (idx + 1) line input
      | '{' -> aux (Token.make_token Token.LeftBrace "{" Token.Null line :: acc) (idx + 1) line input
      | '}' -> aux (Token.make_token Token.RightBrace "}" Token.Null line :: acc) (idx + 1) line input
      | ',' -> aux (Token.make_token Token.Comma "," Token.Null line :: acc) (idx + 1) line input
      | '.' -> aux (Token.make_token Token.Dot "." Token.Null line :: acc) (idx + 1) line input
      | '-' -> aux (Token.make_token Token.Minus "-" Token.Null line :: acc) (idx + 1) line input
      | '+' -> aux (Token.make_token Token.Plus "+" Token.Null line :: acc) (idx + 1) line input
      | ';' -> aux (Token.make_token Token.Semicolon ";" Token.Null line :: acc) (idx + 1) line input
      | '*' -> aux (Token.make_token Token.Star "*" Token.Null line :: acc) (idx + 1) line input
      | '!' ->
          if input.[idx] = '=' then
            aux (Token.make_token Token.BangEqual "!=" Token.Null line :: acc) (idx + 2) line input
          else if input.[idx] = '<' then
            aux (Token.make_token Token.LessEqual "<=" Token.Null line :: acc) (idx + 2) line input
          else if input.[idx] = '>' then
            aux (Token.make_token Token.GreaterEqual ">=" Token.Null line :: acc) (idx + 2) line input
        else aux (Token.make_token Token.Bang "!" Token.Null line :: acc) (idx + 1) line input
      | _ ->
          Error.error line "unexpected character";
          acc
  in
  aux [] 0 1 input
