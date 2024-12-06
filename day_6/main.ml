open Core

type direction = Up | Right | Down | Left

type visit_data = {
  mutable up : bool;
  mutable right : bool;
  mutable down : bool;
  mutable left : bool;
}

let vis_any_direction data = data.up || data.right || data.down || data.left

let find_starting_position input =
  let x = ref 0 in
  let y = ref 0 in
  let height = Array.length input in
  let width = Array.length input.(0) in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.equal input.(i).(j) '^' then (
        y := i;
        x := j)
    done
  done;
  (!x, !y)

let get_all_visited input additional_obstacle =
  let x_beg, y_beg = find_starting_position input in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let visited =
    Array.init height ~f:(fun _ ->
        Array.init width ~f:(fun _ ->
            { up = false; right = false; down = false; left = false }))
  in
  let found_cycle = ref false in
  let x = ref x_beg in
  let y = ref y_beg in
  let dir = ref Up in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let is_obstacle x y =
    Char.equal input.(y).(x) '#'
    ||
    match additional_obstacle with
    | None -> false
    | Some (ax, ay) -> equal x ax && equal y ay
  in
  while (not !found_cycle) && is_safe !x !y do
    match !dir with
    | Up ->
        if visited.(!y).(!x).up then found_cycle := true;
        visited.(!y).(!x).up <- true;
        if is_safe !x (!y - 1) && is_obstacle !x (!y - 1) then dir := Right
        else Int.decr y
    | Right ->
        if visited.(!y).(!x).right then found_cycle := true;
        visited.(!y).(!x).right <- true;
        if is_safe (!x + 1) !y && is_obstacle (!x + 1) !y then dir := Down
        else Int.incr x
    | Down ->
        if visited.(!y).(!x).down then found_cycle := true;
        visited.(!y).(!x).down <- true;
        if is_safe !x (!y + 1) && is_obstacle !x (!y + 1) then dir := Left
        else Int.incr y
    | Left ->
        if visited.(!y).(!x).left then found_cycle := true;
        visited.(!y).(!x).left <- true;
        if is_safe (!x - 1) !y && is_obstacle (!x - 1) !y then dir := Up
        else Int.decr x
  done;
  (!found_cycle, visited)

let solve_1 input =
  let _, marked = get_all_visited input None in
  let res = ref 0 in
  Array.iter marked ~f:(fun row ->
      Array.iter row ~f:(fun x -> if vis_any_direction x then Int.incr res));
  printf "%d\n" !res

let solve_2 input =
  let x_beg, y_beg = find_starting_position input in
  let res = ref 0 in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let _, marked = get_all_visited input None in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if
        vis_any_direction marked.(i).(j)
        && (not (equal i y_beg) || not (equal j x_beg))
      then
        let cycle, _ = get_all_visited input (Some (j, i)) in
        if cycle then Int.incr res
    done
  done;
  printf "%d\n" !res

let parse_input lines = List.to_array lines |> Array.map ~f:String.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
