open Core

type direction = Up | Right | Down | Left

let solve_1 input = 
  let x = ref 0 in
  let y = ref 0 in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let res = ref 0 in
  let dir = ref Up in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.equal input.(i).(j) '^' then (
        y := i;
        x := j
      )
    done
  done;
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  while is_safe !x !y do
    if not @@ Char.equal input.(!y).(!x) 'X' then Int.incr res;
    input.(!y).(!x) <- 'X';
    match !dir with
    | Up -> (
      if is_safe !x (!y - 1) && Char.equal input.(!y - 1).(!x) '#' then
        dir := Right
      else Int.decr y
    )
    | Right -> (if is_safe (!x + 1) !y && Char.equal input.(!y).(!x + 1) '#' then
        dir := Down
      else Int.incr x)
    | Down -> (if is_safe !x (!y + 1) && Char.equal input.(!y + 1).(!x) '#' then
        dir := Left
      else Int.incr y)
    | Left -> (if is_safe (!x - 1) !y && Char.equal input.(!y).(!x - 1) '#' then
        dir := Up
      else Int.decr x)
  done;
  printf "%d\n" !res

let solve_2 _input = ()

let parse_input lines = List.to_array lines |> Array.map ~f:String.to_array

let () = Aoc.run_day solve_1 solve_2 parse_input
