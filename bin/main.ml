let () =
  if Array.length Sys.argv - 1 = 1 then Lox.Repl.run_file Sys.argv.(1)
  else Lox.Repl.run_prompt ()
