module Lox.Repl

open System.IO

let read_file file = List.ofSeq (File.ReadLines(file))

let run contents =
    let tokens = Scanner.scann contents
    let expressions = Parser.parse tokens
    List.iter (fun token -> printfn "%s" (Token.string_of_token token)) tokens
    List.iter (fun literal -> printfn "%s" (Token.string_of_literal literal)) (List.map Evaluator.evaluate expressions)

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
