module Lox.Repl

open System.IO

let read_file file = List.ofSeq (File.ReadLines(file))

let run contents =
    let tokens = Scanner.scann contents
    let statements = Parser.parse tokens
    // List.iter (fun token -> printfn "%s" (string token)) tokens
    List.iter (fun stmt -> printfn "%s" (string stmt)) statements
//List.iter Evaluator.evaluate_stament statements

let run_file file =
    let file_contents = read_file file in List.iter run file_contents

let rec run_prompt () =
    let try_read () =
        match System.Console.ReadLine() with
        | "exit" -> None
        | other -> Some(other)

    match try_read () with
    | Some line ->
        run line
        run_prompt ()
    | None -> ()
