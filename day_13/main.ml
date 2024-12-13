open Core

type point = { x : int; y : int }
type machine = { but_a : point; but_b : point; prize : point }

let print_machine m =
  printf "Machine:\nA -> %d %d\nB -> %d %d\nPrize -> %d %d\n\n" m.but_a.x
    m.but_a.y m.but_b.x m.but_b.y m.prize.x m.prize.y

let get_best_score m =
  let rec aux b_presses x y =
    if x % m.but_a.x = 0 && y % m.but_a.y = 0 && x / m.but_a.x = y / m.but_a.y
    then
      min
        (b_presses + (3 * (x / m.but_a.x)))
        (aux (b_presses + 1) (x - m.but_b.x) (y - m.but_b.y))
    else if x - m.but_b.x >= 0 && y - m.but_b.y >= 0 then
      aux (b_presses + 1) (x - m.but_b.x) (y - m.but_b.y)
    else Int.max_value
  in
  let res = aux 0 m.prize.x m.prize.y in
  if res = Int.max_value then 0 else res

let solve_1 input =
  List.iter input ~f:(fun m -> print_machine m);
  let res = List.fold input ~init:0 ~f:(fun acc m -> acc + get_best_score m) in
  printf "%d\n" res

let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let lines_combined = String.concat ~sep:" " lines in
  let but_a_exp =
    seq
      [ str "Button A: X+"; group (rep1 digit); str ", Y+"; group (rep1 digit) ]
  in
  let but_b_exp =
    seq
      [ str "Button B: X+"; group (rep1 digit); str ", Y+"; group (rep1 digit) ]
  in
  let prize_exp =
    seq [ str "Prize: X="; group (rep1 digit); str ", Y="; group (rep1 digit) ]
  in
  let group_to_point g =
    { x = Int.of_string (Group.get g 1); y = Int.of_string (Group.get g 2) }
  in
  let but_a_matches =
    all (compile but_a_exp) lines_combined |> List.map ~f:group_to_point
  in
  let but_b_matches =
    all (compile but_b_exp) lines_combined |> List.map ~f:group_to_point
  in
  let prize_matches =
    all (compile prize_exp) lines_combined |> List.map ~f:group_to_point
  in
  List.zip_exn but_a_matches but_b_matches
  |> List.zip_exn prize_matches
  |> List.map ~f:(fun (c, (a, b)) -> { but_a = a; but_b = b; prize = c })

let () = Aoc.run_day solve_1 solve_2 parse_input
