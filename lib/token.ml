type tokentype =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Indentifier
  | String
  | Number
  | And
  | Class
  | Else
  | False
  | Fun
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF

type literal = String of string | Number of float | Null
type token = {token: tokentype; lexeme: string; literal: literal; line: int}

let string_of_tokentype = function
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  | Bang -> "!"
  | BangEqual -> "!="
  | Equal -> "="
  | EqualEqual -> "=="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Less -> "<"
  | LessEqual -> "<="
  | Indentifier -> "Indentifier"
  | String -> "String"
  | Number -> "Number"
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  | EOF -> "EOF"

let string_of_literal = function String s -> s | Number n -> string_of_float n | Null -> "null"

let string_of_token = function
  | {token; lexeme; literal; _} ->
  Printf.sprintf "%s %s %s"
    (string_of_tokentype token)
    lexeme
    (string_of_literal literal)

let make_token token lexeme literal line = {token; lexeme; literal; line}
