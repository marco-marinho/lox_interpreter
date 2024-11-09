open Lox

[<EntryPoint>]
let main args =
    if args.Length > 1 then
        Repl.run_file args.[1] |> ignore
    else
        Repl.run_prompt [ Map.empty ]

    0
