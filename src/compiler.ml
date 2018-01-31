(* compiler.ml*)

let arg_list =
    [ ("-length",
    Arg.Rest (fun str -> print_endline (string_of_int (String.length str))),
    "Prints the length of all arguments");
    ]

let usage_msg =
    "Usage: compiler [flags] [args]\nAvailable flags:"

let main () =
    Arg.parse (Arg.align arg_list) print_endline usage_msg

let _ = if !Sys.interactive then () else main ()
