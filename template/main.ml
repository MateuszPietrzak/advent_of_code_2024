open Core

let solve_1 _input = ()

let solve_2 _input = ()

let () =
  let open List.Monad_infix in
  let input =
    In_channel.read_lines "template/input.in"
    >>| (fun line -> Int.of_string line)
  in
  Aoc.run_day solve_1 solve_2 input
