let report line where message =
  Printf.printf "[line %d] Error %s: %s" line where message

let error line message = report line "" message
