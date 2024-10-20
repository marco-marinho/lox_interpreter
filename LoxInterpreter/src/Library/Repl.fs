module Lox.Repl

open Lox
open System.IO

let read_file file = List.ofSeq (File.ReadLines(file))

let run contents =
    let tokens = Scanner.scann contents
    List.iter (fun token -> printfn "%s" (Token.string_of_token token)) tokens

// let run contents =
//     let tokens = Scanner.scanner contents in
//     let expressions = Parser.parse tokens in
//     List.iter (fun expr -> print_endline (Expression.string_of_expression expr)) expressions
//     List.iter (fun literal -> print_string (Token.string_of_literal literal)) (List.map Evaluator.evaluate expressions)

let run_file file =
    let file_contents = read_file file in List.iter run file_contents

let rec run_prompt () =
    let try_read () =
        match System.Console.ReadLine() with
        | "exit()" -> None
        | other -> Some(other)

    match try_read () with
    | Some line ->
        run line
        run_prompt ()
    | None -> ()
