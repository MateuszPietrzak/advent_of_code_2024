open Core

type point = { x : int; y : int }
type machine = { but_a : point; but_b : point; prize : point }

let get_best_score m =
  let w = m.but_a.x * m.but_b.y - m.but_a.y * m.but_b.x in
  let w_a = m.prize.x * m.but_b.y - m.prize.y * m.but_b.x in
  let w_b = m.but_a.x * m.prize.y - m.but_a.y * m.prize.x in
  if w <> 0 && ((abs w_a) % (abs w)) = 0 && ((abs w_b) % (abs w)) = 0 then
    let a = w_a / w in
    let b = w_b / w in
    a * 3 + b
  else 0

let solve_1 input =
  let res = List.fold input ~init:0 ~f:(fun acc m -> acc + get_best_score m) in
  printf "%d\n" res

let solve_2 input =
  let scaled_input =
    List.map input ~f:(fun m ->
        {
          m with
          prize =
            { x = m.prize.x + 10000000000000; y = m.prize.y + 10000000000000 };
        })
  in
  let res =
    List.fold scaled_input ~init:0 ~f:(fun acc m -> acc + get_best_score m)
  in
  printf "%d\n" res

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
