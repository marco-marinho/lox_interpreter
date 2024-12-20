module Lox.Environment

type value =
    | Value of Token.literal
    | Fun of Callable.callable

let value_wrap v = Value v

let define env key value =
    match env with
    | h :: t -> Map.add key value h :: t
    | [] -> failwith "No environment to define variable"

let rec assign env key value =
    let rec aux env acc =
        match env with
        | [] -> failwith (sprintf "Undefined variable '%s'" key)
        | h :: t ->
            match Map.tryFind key h with
            | Some _ -> List.rev acc @ (Map.add key value h :: t)
            | None -> aux t (h :: acc)

    aux env []

let rec get env key =
    match env with
    | [] -> failwith (sprintf "Undefined variable '%s'" key)
    | h :: t ->
        match Map.tryFind key h with
        | Some value -> value
        | None -> get t key
