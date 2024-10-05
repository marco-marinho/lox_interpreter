let read_file file =
  let fi = open_in file in
  let try_read () = try Some (input_line fi) with End_of_file -> None in
  let rec aux acc =
    match try_read () with
    | Some line -> aux acc ^ line
    | None ->
        close_in fi;
        acc
  in
  aux ""

let run contents =
  let tokens = Scanner.scanner contents in
  List.iter (fun x -> Printf.printf "%s, " (Token.string_of_token x)) tokens; Printf.printf "\n"

let run_file file =
  let file_contents = read_file file in
  run file_contents

let rec run_prompt () =
  let try_read () = try Some (read_line ()) with End_of_file -> None in
  match try_read () with
  | Some line ->
      run line;
      run_prompt ()
  | None -> ()
