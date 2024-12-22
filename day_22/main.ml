open! Core

let mix x y = Int.bit_xor x y
let prune x = x % 16777216

let solve_1 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc x ->
        let rec aux num depth =
          match depth with
          | 0 -> num
          | d ->
              let v1 = prune @@ mix (num * 64) num in
              let v2 = prune @@ mix (v1 / 32) v1 in
              let v3 = prune @@ mix (v2 * 2048) v2 in
              aux v3 (d - 1)
        in
        let res = aux x 2000 in
        acc + res)
  in
  printf "%d\n" res

let solve_2 _input = ()
let parse_input lines = lines |> List.map ~f:Int.of_string
let () = Aoc.run_day solve_1 solve_2 parse_input
