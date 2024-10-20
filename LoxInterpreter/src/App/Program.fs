open Lox

[<EntryPoint>]
let main args =
    if Array.length args = 0 then
        Repl.run_prompt ()
        0
    else
        Repl.run_file args.[0]
        0
