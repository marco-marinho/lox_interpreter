module Lox.Environment

type environment_entry =
    | Value of Token.literal
    | Callable of Statement.statement

let define_var env key value =
    let value = Value(value)

    match env with
    | h :: t -> Map.add key value h :: t
    | [] -> failwith "No environment to define variable"

let define_fun env key value =
    let value = Callable(value)

    match env with
    | h :: t -> Map.add key value h :: t
    | [] -> failwith "No environment to define variable"

let rec assign_var env key value =
    let value = Value(value)

    let rec aux env acc =
        match env with
        | [] -> failwith (sprintf "Undefined variable '%s'" key)
        | h :: t ->
            match Map.tryFind key h with
            | Some _ -> List.rev acc @ (Map.add key value h :: t)
            | None -> aux t (h :: acc)

    aux env []

let rec get_var env key =
    match env with
    | [] -> failwith (sprintf "Undefined variable '%s'" key)
    | h :: t ->
        match Map.tryFind key h with
        | Some value ->
            match value with
            | Value v -> v
            | _ -> failwith (sprintf "Variable '%s' is not a literal" key)
        | None -> get_var t key

let rec get_fun env key =
    match env with
    | [] -> failwith (sprintf "Undefined function or method '%s'" key)
    | h :: t ->
        match Map.tryFind key h with
        | Some value ->
            match value with
            | Callable c -> c
            | _ -> failwith (sprintf "'%s' is not a function and cannot be applied" key)
        | None -> get_fun t key
