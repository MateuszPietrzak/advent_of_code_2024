open Core

let dfs input is_safe x y =
  let deltas = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  let rec aux x y =
    let area = ref 1 in
    let perim = ref 0 in
    let cur_char = input.(y).(x) in
    input.(y).(x) <- '#';
    List.iter deltas ~f:(fun (dx, dy) ->
        if
          is_safe (x + dx) (y + dy)
          && Char.( = ) input.(y + dy).(x + dx) cur_char
        then (
          let a, p = aux (x + dx) (y + dy) in
          area := !area + a;
          perim := !perim + p)
        else if
          (not @@ is_safe (x + dx) (y + dy))
          || Char.( <> ) input.(y + dy).(x + dx) cur_char
             && Char.( <> ) input.(y + dy).(x + dx) '#'
        then Int.incr perim);
    (!area, !perim)
  in
  let rec aux2 x y =
    input.(y).(x) <- '.';
    List.iter deltas ~f:(fun (dx, dy) ->
        if is_safe (x + dx) (y + dy) && Char.( = ) input.(y + dy).(x + dx) '#'
        then aux2 (x + dx) (y + dy))
  in
  let a, p = aux x y in
  aux2 x y;
  a * p

let dfs2 input is_safe x y =
  let deltas = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let cur_char = input.(y).(x) in
  let rec aux3 x y =
    input.(y).(x) <- '-';
    List.iter deltas ~f:(fun (dx, dy) ->
        if
          is_safe (x + dx) (y + dy)
          && Char.( = ) input.(y + dy).(x + dx) cur_char
        then aux3 (x + dx) (y + dy))
  in
  aux3 x y;
  let corners_taken =
    Array.init (height + 1) ~f:(fun _ -> Array.init (width + 1) ~f:(const 0))
  in
  let rec aux x y =
    let area = ref 1 in
    let cur_char = input.(y).(x) in
    input.(y).(x) <- '#';
    let get_corner x y dx dy =
      if
        ((not @@ is_safe (x + dx) y)
        || Char.( <> ) input.(y).(x + dx) cur_char
           && Char.( <> ) input.(y).(x + dx) '#')
        && ((not @@ is_safe x (y + dy))
           || Char.( <> ) input.(y + dy).(x) cur_char
              && Char.( <> ) input.(y + dy).(x) '#')
      then
        if
          is_safe (x + dx) (y + dy)
          && (Char.( = ) input.(y + dy).(x + dx) cur_char
             || Char.( = ) input.(y + dy).(x + dx) '#')
        then 2
        else 1
      else if
        is_safe (x + dx) y
        && is_safe x (y + dy)
        && (Char.( = ) input.(y).(x + dx) cur_char
           || Char.( = ) input.(y).(x + dx) '#')
        && (Char.( = ) input.(y + dy).(x) cur_char
           || Char.( = ) input.(y + dy).(x) '#')
        && Char.( <> ) input.(y + dy).(x + dx) cur_char
        && Char.( <> ) input.(y + dy).(x + dx) '#'
      then 1
      else 0
    in
    let deltas2 = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ] in
    List.iter deltas2 ~f:(fun (dx, dy) ->
        let sdx = if dx = -1 then 0 else 1 in
        let sdy = if dy = -1 then 0 else 1 in
        corners_taken.(y + sdy).(x + sdx) <-
          max corners_taken.(y + sdy).(x + sdx) (get_corner x y dx dy));

    List.iter deltas ~f:(fun (dx, dy) ->
        if
          is_safe (x + dx) (y + dy)
          && Char.( = ) input.(y + dy).(x + dx) cur_char
        then
          let a = aux (x + dx) (y + dy) in
          area := !area + a);
    !area
  in
  let rec aux2 x y =
    input.(y).(x) <- '.';
    List.iter deltas ~f:(fun (dx, dy) ->
        if is_safe (x + dx) (y + dy) && Char.( = ) input.(y + dy).(x + dx) '#'
        then aux2 (x + dx) (y + dy))
  in
  let a = aux x y in
  let p =
    Array.fold corners_taken ~init:0 ~f:(fun acc row ->
        acc + Array.fold row ~init:0 ~f:( + ))
  in
  aux2 x y;
  a * p

let solve_1 input =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.( <> ) input.(i).(j) '.' then res := !res + dfs input is_safe j i
    done
  done;
  printf "%d\n" !res

let solve_2 input =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.( <> ) input.(i).(j) '.' then res := !res + dfs2 input is_safe j i
    done
  done;
  printf "%d\n" !res

let parse_input lines = List.to_array lines |> Array.map ~f:String.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
