module Lox.Callable

open System

type callable =
    | Function of string * list<Token.token> * Statement.statement
    | Other of string

let clock _ env =
    Expression.LiteralExpr(Token.NumberLiteral(DateTimeOffset.Now.ToUnixTimeMilliseconds() |> float)), env
