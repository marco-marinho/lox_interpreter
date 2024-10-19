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
    | For
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

type literal =
    | StringLiteral of string
    | NumberLiteral of float
    | Null
    | BoolLiteral of bool

type token =
    { token: tokentype
      lexeme: string
      literal: literal
      line: int }

let (|TOKEN|) (t: token) = t.token, t.lexeme, t.literal, t.line

let string_of_tokentype =
    function
    | LeftParen -> "LeftParenthesis"
    | RightParen -> "RightParenthesis"
    | LeftBrace -> "LeftBrace"
    | RightBrace -> "RightBrace"
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Minus -> "Minus"
    | Plus -> "Plus"
    | Semicolon -> "Semiconlon"
    | Slash -> "Slash"
    | Star -> "Star"
    | Bang -> "Bang"
    | BangEqual -> "BandEqual"
    | Equal -> "Equal"
    | EqualEqual -> "EqualEqual"
    | Greater -> "Greater"
    | GreaterEqual -> "GreaterEqual"
    | Less -> "Less"
    | LessEqual -> "LessEqual"
    | Indentifier -> "Indentifier"
    | String -> "String"
    | Number -> "Number"
    | And -> "and"
    | Class -> "class"
    | Else -> "else"
    | False -> "false"
    | Fun -> "fun"
    | For -> "for"
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

let string_of_literal =
    function
    | StringLiteral s -> s
    | NumberLiteral n -> string n
    | Null -> "null"
    | BoolLiteral b -> string b

let string_of_token =
    function
    | TOKEN(token, lexeme, literal, _) ->
        Printf.sprintf
            "{Token: %s, Lexeme: %s, Literal: %s}"
            (string_of_tokentype token)
            lexeme
            (string_of_literal literal)

let make_token token lexeme literal line =
    { token = token
      lexeme = lexeme
      literal = literal
      line = line }

let token_type t = t.token

let token_literal t = t.literal
