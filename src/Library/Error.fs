module Lox.Error

let report line where message =
    printfn $"[line {line}] Error {where}: {message}"

let error line message = report line "" message
