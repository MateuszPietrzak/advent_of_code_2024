open Core

let rec is_possible (res, equation) =
  match equation with
  | [] -> 0
  | [ x ] -> if equal x res then x else 0
  | x :: y :: xs ->
      let add = is_possible (res, (x + y) :: xs) in
      if not @@ equal add 0 then add else is_possible (res, (x * y) :: xs)

let solve_1 input =
  let res = List.map input ~f:is_possible |> List.fold ~init:0 ~f:( + ) in
  printf "%d\n" res

let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let exp =
    seq
      [
        group (rep1 digit);
        str ": ";
        group (rep1 @@ seq [ rep1 digit; opt (str " ") ]);
      ]
  in
  let re = compile exp in

  List.map lines ~f:(fun line ->
      let g = Re.exec re line in
      let res_value = Int.of_string (Group.get g 1) in
      let equation =
        Group.get g 2 |> String.split ~on:' ' |> List.map ~f:Int.of_string
      in
      (res_value, equation))

let () = Aoc.run_day solve_1 solve_2 parse_input
