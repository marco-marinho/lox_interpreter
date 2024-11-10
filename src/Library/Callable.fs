module Lox.Callable

open System

type lox_callable =
    { call: Token.token list -> Map<string, Token.literal> -> Expression.expression * Map<string, Token.literal> }


let clock _ env =
    Expression.LiteralExpr(Token.NumberLiteral(DateTimeOffset.Now.ToUnixTimeMilliseconds() |> float)), env

let a = { call = clock }
