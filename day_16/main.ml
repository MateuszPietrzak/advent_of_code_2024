open Core

type direction = Up | Right | Down | Left
type point = { mutable x : int; mutable y : int }

type cost_cell = {
  mutable up : int;
  mutable right : int;
  mutable down : int;
  mutable left : int;
}

module HeapElem = struct
  type t = { cost : int; pos : point; dir : direction }

  let compare t1 t2 = compare t1.cost t2.cost
end

module Q = Binary_heap.Make (HeapElem)

let get_delta = function
  | Up -> { x = 0; y = -1 }
  | Right -> { x = 1; y = 0 }
  | Down -> { x = 0; y = 1 }
  | Left -> { x = -1; y = 0 }

let turn_left = function
  | Up -> Left
  | Right -> Up
  | Down -> Right
  | Left -> Down

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let get_cost cell = function
  | Up -> cell.up
  | Right -> cell.right
  | Down -> cell.down
  | Left -> cell.left

let set_cost cell cost = function
  | Up -> cell.up <- cost
  | Right -> cell.right <- cost
  | Down -> cell.down <- cost
  | Left -> cell.left <- cost

let solve_1 input =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let start_p = { x = 0; y = 0 } in
  let end_p = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = 'S') then (
        start_p.x <- j;
        start_p.y <- i;
        input.(i).(j) <- '.')
      else if Char.(input.(i).(j) = 'E') then (
        end_p.x <- j;
        end_p.y <- i;
        input.(i).(j) <- '.')
    done
  done;
  let q = Q.create ~dummy:{ cost = 0; pos = start_p; dir = Right } 1 in
  Q.add q { cost = 0; pos = start_p; dir = Right };

  let dist =
    Array.init height ~f:(fun _ ->
        Array.init width
          ~f:
            (fun _ ->
               {
                 up = Int.max_value;
                 right = Int.max_value;
                 down = Int.max_value;
                 left = Int.max_value;
               }))
  in
  set_cost dist.(start_p.y).(start_p.x) 0 Right;

  while not @@ Q.is_empty q do
    let top = Q.pop_minimum q in
    let delta = get_delta top.dir in
    if
      Char.(input.(top.pos.y + delta.y).(top.pos.x + delta.x) = '.')
      && get_cost dist.(top.pos.y + delta.y).(top.pos.x + delta.x) top.dir
         > top.cost + 1
    then (
      Q.add q
        {
          cost = top.cost + 1;
          pos = { y = top.pos.y + delta.y; x = top.pos.x + delta.x };
          dir = top.dir;
        };
      set_cost
        dist.(top.pos.y + delta.y).(top.pos.x + delta.x)
        (top.cost + 1) top.dir);
    if
      get_cost dist.(top.pos.y).(top.pos.x) (turn_left top.dir)
      > top.cost + 1000
    then (
      Q.add q
        {
          cost = top.cost + 1000;
          pos = { y = top.pos.y; x = top.pos.x };
          dir = turn_left top.dir;
        };
      set_cost
        dist.(top.pos.y).(top.pos.x)
        (top.cost + 1000) (turn_left top.dir));
    if
      get_cost dist.(top.pos.y).(top.pos.x) (turn_right top.dir)
      > top.cost + 1000
    then (
      Q.add q
        {
          cost = top.cost + 1000;
          pos = { y = top.pos.y; x = top.pos.x };
          dir = turn_right top.dir;
        };
      set_cost
        dist.(top.pos.y).(top.pos.x)
        (top.cost + 1000) (turn_right top.dir))
  done;
  let res = List.fold [Up; Right; Down; Left] ~init:Int.max_value ~f:(fun acc x -> min acc (get_cost dist.(end_p.y).(end_p.x) x)) in
  printf "%d\n" res

let solve_2 _input = ()
let parse_input lines = List.map lines ~f:String.to_array |> List.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
