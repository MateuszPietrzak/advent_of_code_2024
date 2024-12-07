open Core

let rec is_possible res equation operators =
  match equation with
  | [] -> 0
  | [ x ] -> if equal x res then x else 0
  | x :: y :: xs ->
      List.fold operators ~init:0 ~f:(fun acc op ->
          if not @@ equal acc 0 then acc
          else
            let rs = is_possible res (op x y :: xs) operators in
            rs)

let solve_1 input =
  let res =
    List.map input ~f:(fun (a, b) -> is_possible a b [ ( + ); ( * ) ])
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" res

let solve_2 input =
  let res =
    List.map input ~f:(fun (a, b) ->
        is_possible a b
          [
            ( + );
            ( * );
            (fun x y ->
              let str1 = Int.to_string x in
              let str2 = Int.to_string y in
              Int.of_string (str1 ^ str2));
          ])
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" res

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
