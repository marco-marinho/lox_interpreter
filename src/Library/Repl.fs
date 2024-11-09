module Lox.Repl

open System.IO

let read_file file =
    String.concat " " (List.ofSeq (File.ReadLines(file)))

let run contents environment =
    let tokens = Scanner.scann contents
    let statements = Parser.parse tokens
    // List.iter (fun token -> printfn "%s" (string token)) tokens
    List.iter (fun stmt -> printfn "%s" (string stmt)) statements
    List.fold (fun env x -> Evaluator.evaluate_stament x env) environment statements

let run_file file =
    let tokens = Scanner.scann (read_file file)
    let statements = Parser.parse tokens
    List.fold (fun env x -> Evaluator.evaluate_stament x env) [ Map.empty ] statements

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
