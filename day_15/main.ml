open Core

type point = { mutable x : int; mutable y : int }
type move = Up | Right | Down | Left

let move_get_new_pos board pos move =
  let d =
    match move with
    | Up -> { x = 0; y = -1 }
    | Right -> { x = 1; y = 0 }
    | Down -> { x = 0; y = 1 }
    | Left -> { x = -1; y = 0 }
  in
  if Char.(board.(pos.y + d.y).(pos.x + d.x) = '.') then
    { x = pos.x + d.x; y = pos.y + d.y }
  else if Char.(board.(pos.y + d.y).(pos.x + d.x) = '#') then pos
  else if Char.(board.(pos.y + d.y).(pos.x + d.x) = 'O') then (
    let f_pos = { x = pos.x + d.x; y = pos.y + d.y } in
    while Char.(board.(f_pos.y).(f_pos.x) = 'O') do
      f_pos.x <- f_pos.x + d.x;
      f_pos.y <- f_pos.y + d.y
    done;
    match board.(f_pos.y).(f_pos.x) with
    | '#' -> pos
    | '.' ->
        board.(f_pos.y).(f_pos.x) <- 'O';
        board.(pos.y + d.y).(pos.x + d.x) <- '.';
        { x = pos.x + d.x; y = pos.y + d.y }
    | _ -> assert false)
  else assert false

let solve_1 (board, moves) =
  let height = Array.length board in
  let width = Array.length board.(0) in
  let pos = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(board.(i).(j) = '@') then (
        pos.x <- j;
        pos.y <- i;
        board.(i).(j) <- '.')
    done
  done;
  List.iter moves ~f:(fun m -> (
    let new_pos = move_get_new_pos board pos m in
    pos.x <- new_pos.x;
    pos.y <- new_pos.y
  ));
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(board.(i).(j) = 'O') then res := !res + (100 * i) + j
    done
  done;
  printf "%d\n" !res

let solve_2 _input = ()

let parse_input lines =
  let combined = String.concat ~sep:" " lines in
  let open Re in
  let exp1 = group @@ rep1 @@ alt [ char '#'; char '.'; char '@'; char 'O' ] in
  let exp2 = group @@ rep1 @@ alt [ char '^'; char 'v'; char '<'; char '>' ] in
  let match1 =
    all (compile exp1) combined
    |> List.map ~f:(fun g -> Group.get g 1 |> String.to_array)
    |> List.to_array
  in
  let match2 =
    all (compile exp2) combined
    |> List.map ~f:(fun g -> Group.get g 1)
    |> String.concat |> String.to_list
    |> List.map ~f:(fun c ->
           match c with
           | '^' -> Up
           | '>' -> Right
           | 'v' -> Down
           | '<' -> Left
           | _ -> assert false)
  in

  (match1, match2)

let () = Aoc.run_day solve_1 solve_2 parse_input
