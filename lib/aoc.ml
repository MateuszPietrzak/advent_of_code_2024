open Core

let run_day solve_1 solve_2 parse_input =
  if Array.length (Sys.get_argv ()) <> 3 then
    printf
      "Invalid usage: Please specify the input file path and the part number.\n"
  else
    let arg_part = Int.of_string_opt (Sys.get_argv ()).(2) in
    match arg_part with
    | None -> printf "Invalid usage: Could not read the part.\n"
    | Some part -> (
        let arg_file = (Sys.get_argv ()).(1) in
        try
          let lines = In_channel.read_lines arg_file in
          let t1 = Time_ns.now () in
          (match part with
          | 1 -> solve_1 (parse_input lines)
          | 2 -> solve_2 (parse_input lines)
          | _ -> printf "Invalid usage: Part %d not allowed.\n" part);
          let t2 = Time_ns.now () in
          let elapsed = Time_ns.diff t2 t1 in
          printf "Elapsed running time: %.6f\n" (Time_ns.Span.to_sec elapsed)
        with Sys_error _ -> printf "Invalid usage: Could not read the file.\n")

let int_list_of_line line =
  let split_list = String.split_on_chars line ~on:[ ' '; '\n'; '\t' ] in
  let trimmed =
    List.filter split_list ~f:(fun x -> match x with "" -> false | _ -> true)
  in
  List.map trimmed ~f:Int.of_string
