module Lox.Callable

open System

let clock _ env =
    Expression.LiteralExpr(Token.NumberLiteral(DateTimeOffset.Now.ToUnixTimeMilliseconds() |> float)), env

