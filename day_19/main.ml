open Core

let check available_patterns pattern =
  let rec aux = function
    | "" -> true
    | str ->
        List.fold available_patterns ~init:false ~f:(fun acc ap ->
            if acc then true
            else if String.length str >= String.length ap then
              let l = String.length ap in
              if String.(equal (prefix str l) ap) then
                aux (String.drop_prefix str l)
              else false
            else false)
  in
  aux pattern

let memo = Hashtbl.create (module String)

let check_2 available_patterns pattern =
  let rec aux = function
    | "" -> 1
    | str -> (
        match Hashtbl.find memo str with
        | None ->
            let res =
              List.fold available_patterns ~init:0 ~f:(fun acc ap ->
                  if String.length str >= String.length ap then
                    let l = String.length ap in
                    if String.(equal (prefix str l) ap) then
                      acc + aux (String.drop_prefix str l)
                    else acc
                  else acc)
            in
            Hashtbl.set memo ~key:str ~data:res;
            res
        | Some res -> res)
  in
  aux pattern

let solve_1 (available_patterns, to_check) =
  printf "%d\n"
    (List.fold to_check ~init:0 ~f:(fun acc p ->
         if check available_patterns p then acc + 1 else acc))

let solve_2 (available_patterns, to_check) =
  printf "%d\n"
    (List.fold to_check ~init:0 ~f:(fun acc p ->
         acc + check_2 available_patterns p))

let parse_input lines =
  let available_patterns =
    List.hd_exn lines
    |> String.split_on_chars ~on:[ ','; ' ' ]
    |> List.filter ~f:(function "" -> false | _ -> true)
  in

  let to_check = List.tl_exn @@ List.tl_exn lines in
  (available_patterns, to_check)

let () = Aoc.run_day solve_1 solve_2 parse_input
