module Lox.Repl

open System.IO

let read_file file = List.ofSeq (File.ReadLines(file))

let run contents environment =
    let tokens = Scanner.scann contents
    let statements = Parser.parse tokens
    // List.iter (fun token -> printfn "%s" (string token)) tokens
    List.iter (fun stmt -> printfn "%s" (string stmt)) statements
    List.fold (fun env x -> Evaluator.evaluate_stament x env) environment statements

// let run_file file =
//     let file_contents = read_file file in List.iter run file_contents

let rec run_prompt environment =
    let try_read () =
        match System.Console.ReadLine() with
        | "exit"
        | "quit" -> None
        | other -> Some(other)

    match try_read () with
    | Some line ->
        let environment = run line environment
        run_prompt environment
    | None -> ()
