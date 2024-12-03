open Core

let solve_1 input =
  let open Re in
  let re =
    compile
      (seq
         [
           str "mul("; group (rep1 digit); str ","; group (rep1 digit); str ")";
         ])
  in
  let matches =
    all re input
    |> List.map ~f:(fun g ->
           (Int.of_string (Group.get g 1), Int.of_string (Group.get g 2)))
  in
  let result = List.fold matches ~init:0 ~f:(fun acc (a, b) -> acc + (a * b)) in
  printf "%d\n" result

let solve_2 input =
  let open Re in
  let mul_exp =
    seq [ str "mul("; group (rep1 digit); str ","; group (rep1 digit); str ")" ]
  in
  let re_all = compile (alt [ mul_exp; str "do()"; str "don't()" ]) in
  let re_mul = compile mul_exp in
  let matches = all re_all input |> List.map ~f:(fun g -> Group.get g 0) in
  let rec acc_result acc enabled list =
    match list with
    | [] -> acc
    | x :: xs -> (
        match x with
        | "do()" -> acc_result acc true xs
        | "don't()" -> acc_result acc false xs
        | str ->
            if not enabled then acc_result acc false xs
            else
              let groups = exec re_mul str in
              let to_add =
                Int.of_string (Group.get groups 1)
                * Int.of_string (Group.get groups 2)
              in
              acc_result (acc + to_add) true xs)
  in
  let result = acc_result 0 true matches in
  printf "%d\n" result

let parse_input lines = 
  lines 
    |> List.fold ~init:"" ~f:(fun acc s -> acc ^ s)

let () =
  Aoc.run_day solve_1 solve_2 parse_input
