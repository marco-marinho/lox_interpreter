open Lox

[<EntryPoint>]
let main args =
    Repl.run_prompt [ Map.empty ]
    0
