open Core

let run_day solve_2 solve_1 input =
  if Array.length (Sys.get_argv ()) <> 2 then
    printf "Invalid usage: Please specify the part.\n"
  else
    let arg_2 = Int.of_string_opt (Sys.get_argv ()).(1) in
    match arg_2 with
    | None -> printf "Invalid usage: Could not read the part.\n"
    | Some part -> (
        match part with
        | 1 -> solve_1 input
        | 2 -> solve_2 input
        | _ -> printf "Invalid usage: Part %d not allowed.\n" part)
