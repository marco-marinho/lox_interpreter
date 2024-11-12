module Lox.Token

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

    override this.ToString() =
        match this with
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

type literal =
    | StringLiteral of string
    | NumberLiteral of float
    | Null
    | BoolLiteral of bool

    override this.ToString() =
        match this with
        | StringLiteral s -> s
        | NumberLiteral n -> string n
        | Null -> "null"
        | BoolLiteral b -> string b

type token =
    { token: tokentype
      lexeme: string
      literal: literal
      line: int }

    override this.ToString() =
        sprintf "{Token: %s, Lexeme: %s, Literal: %s}" (string this.token) this.lexeme (string this.literal)

let make_token token lexeme literal line =
    { token = token
      lexeme = lexeme
      literal = literal
      line = line }

let token_type t = t.token

let token_literal t = t.literal

let token_lexeme t = t.lexeme
