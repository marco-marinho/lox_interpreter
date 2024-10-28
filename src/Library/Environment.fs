module Lox.Environment

let define env key value = Map.add key value env

let get env key =
    match Map.tryFind key env with
    | Some value -> value
    | None -> failwith (sprintf "Undefined variable '%s'" key)
