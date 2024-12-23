open Core

type direction = Up | Right | Down | Left [@@deriving sexp]
type point = { mutable x : int; mutable y : int } [@@deriving sexp]

type prev_cell = {
  mutable back : bool;
  mutable left : bool;
  mutable right : bool;
}

type 'a cost_cell = {
  mutable up : 'a;
  mutable right : 'a;
  mutable down : 'a;
  mutable left : 'a;
}

module HeapElem = struct
  module T = struct
    type t = { cost : int; pos : point; dir : direction } [@@deriving sexp]

    let compare t1 t2 = Int.compare t1.cost t2.cost
  end

  include T
  include Comparator.Make (T)
end

module PriorityQueue = Binary_heap.Make (HeapElem)

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

let dijkstra ~maze ~start =
  let height = Array.length maze and width = Array.length maze.(0) in
  let dists =
    Array.init height ~f:(fun _ ->
        Array.init width ~f:(fun _ ->
            {
              up = Int.max_value;
              right = Int.max_value;
              down = Int.max_value;
              left = Int.max_value;
            }))
  and prev =
    Array.init height ~f:(fun _ ->
        Array.init width ~f:(fun _ ->
            {
              up = { back = false; right = false; left = false };
              right = { back = false; right = false; left = false };
              down = { back = false; right = false; left = false };
              left = { back = false; right = false; left = false };
            }))
  in
  let pq =
    PriorityQueue.create ~dummy:{ cost = 0; pos = start; dir = Right } 1
  in
  PriorityQueue.add pq { cost = 0; pos = start; dir = Right };
  dists.(start.y).(start.x).right <- 0;

  while not @@ PriorityQueue.is_empty pq do
    let top = PriorityQueue.pop_minimum pq in
    let cur_cost = get_cost dists.(top.pos.y).(top.pos.x) top.dir in

    (* Forwards. *)
    let delta = get_delta top.dir in
    (if
       Char.(maze.(top.pos.y + delta.y).(top.pos.x + delta.x) = '.')
       && get_cost dists.(top.pos.y + delta.y).(top.pos.x + delta.x) top.dir
          = cur_cost + 1
     then
       (* Set the current prev direction *)
       match top.dir with
       | Up -> prev.(top.pos.y + delta.y).(top.pos.x + delta.x).up.back <- true
       | Right ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).right.back <- true
       | Down ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).down.back <- true
       | Left ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).left.back <- true);

    if
      Char.(maze.(top.pos.y + delta.y).(top.pos.x + delta.x) = '.')
      && get_cost dists.(top.pos.y + delta.y).(top.pos.x + delta.x) top.dir
         > cur_cost + 1
    then (
      set_cost
        dists.(top.pos.y + delta.y).(top.pos.x + delta.x)
        (cur_cost + 1) top.dir;
      (* Clear the prev of the vertex and set the new one. *)
      set_cost
        prev.(top.pos.y + delta.y).(top.pos.x + delta.x)
        { back = true; right = false; left = false }
        top.dir;
      PriorityQueue.add pq
        {
          cost = cur_cost + 1;
          pos = { x = top.pos.x + delta.x; y = top.pos.y + delta.y };
          dir = top.dir;
        });

    (* Turn right. *)
    let dir_right = turn_right top.dir in
    (if get_cost dists.(top.pos.y).(top.pos.x) dir_right = cur_cost + 1000 then
       (* Set the current prev direction *)
       match top.dir with
       | Up -> prev.(top.pos.y + delta.y).(top.pos.x + delta.x).up.left <- true
       | Right ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).right.left <- true
       | Down ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).down.left <- true
       | Left ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).left.left <- true);
    if get_cost dists.(top.pos.y).(top.pos.x) dir_right > cur_cost + 1000 then (
      set_cost dists.(top.pos.y).(top.pos.x) (cur_cost + 1000) dir_right;
      (* Clear the prev of the vertex. *)
      set_cost
        prev.(top.pos.y).(top.pos.x)
        { back = false; right = false; left = true }
        dir_right;
      PriorityQueue.add pq
        {
          cost = cur_cost + 1000;
          pos = { x = top.pos.x; y = top.pos.y };
          dir = dir_right;
        });

    (* Turn left. *)
    let dir_left = turn_left top.dir in
    (if get_cost dists.(top.pos.y).(top.pos.x) dir_left = cur_cost + 1000 then
       (* Set the current prev direction *)
       match top.dir with
       | Up -> prev.(top.pos.y + delta.y).(top.pos.x + delta.x).up.right <- true
       | Right ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).right.right <- true
       | Down ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).down.right <- true
       | Left ->
           prev.(top.pos.y + delta.y).(top.pos.x + delta.x).left.right <- true);
    if get_cost dists.(top.pos.y).(top.pos.x) dir_left > cur_cost + 1000 then (
      set_cost dists.(top.pos.y).(top.pos.x) (cur_cost + 1000) dir_left;
      (* Clear the prev of the vertex. *)
      set_cost
        prev.(top.pos.y).(top.pos.x)
        { back = false; right = true; left = false }
        dir_left;
      PriorityQueue.add pq
        {
          cost = cur_cost + 1000;
          pos = { x = top.pos.x; y = top.pos.y };
          dir = dir_left;
        })
  done;
  (dists, prev)

let solve_1 input =
  let height = Array.length input and width = Array.length input.(0) in
  let start_point = { x = 0; y = 0 } and end_point = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = 'S') then (
        start_point.x <- j;
        start_point.y <- i;
        input.(i).(j) <- '.');
      if Char.(input.(i).(j) = 'E') then (
        end_point.x <- j;
        end_point.y <- i;
        input.(i).(j) <- '.')
    done
  done;
  let dists, _ = dijkstra ~maze:input ~start:start_point in
  let res =
    List.fold [ Up; Right; Down; Left ] ~init:Int.max_value ~f:(fun acc x ->
        Int.min acc (get_cost dists.(end_point.y).(end_point.x) x))
  in
  printf "%d\n" res

let solve_2 input =
  let height = Array.length input and width = Array.length input.(0) in
  let start_point = { x = 0; y = 0 } and end_point = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = 'S') then (
        start_point.x <- j;
        start_point.y <- i;
        input.(i).(j) <- '.');
      if Char.(input.(i).(j) = 'E') then (
        end_point.x <- j;
        end_point.y <- i;
        input.(i).(j) <- '.')
    done
  done;
  let dists, prev = dijkstra ~maze:input ~start:start_point in
  let q = Queue.create () in

  (* Find a minimum to determine starting backtrack directions. *)
  let res =
    List.fold [ Up; Right; Down; Left ] ~init:Int.max_value ~f:(fun acc x ->
        Int.min acc (get_cost dists.(end_point.y).(end_point.x) x))
  in

  List.iter [ Up; Right; Down; Left ] ~f:(fun d ->
      if get_cost dists.(end_point.y).(end_point.x) d = res then
        Queue.enqueue q (end_point, d));

  let visited =
    Array.init height ~f:(fun _ ->
        Array.init width ~f:(fun _ ->
            { up = false; right = false; down = false; left = false }))
  in
  while not @@ Queue.is_empty q do
    let point, dir = Queue.dequeue_exn q in
    set_cost visited.(point.y).(point.x) true dir;
    let prev_cell = get_cost prev.(point.y).(point.x) dir in
    let delta = get_delta @@ turn_left @@ turn_left dir in
    if
      prev_cell.back
      && not (get_cost visited.(point.y + delta.y).(point.x + delta.x) dir)
    then Queue.enqueue q ({ x = point.x + delta.x; y = point.y + delta.y }, dir);
    if
      prev_cell.left
      && not (get_cost visited.(point.y).(point.x) (turn_left dir))
    then Queue.enqueue q (point, turn_left dir);
    if
      prev_cell.right
      && not (get_cost visited.(point.y).(point.x) (turn_right dir))
    then Queue.enqueue q (point, turn_right dir)
  done;

  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if
        List.fold [ Up; Right; Down; Left ] ~init:false ~f:(fun acc dir ->
            acc || get_cost visited.(i).(j) dir)
      then Int.incr res
    done
  done;

  printf "%d\n" !res

let parse_input lines = List.map lines ~f:String.to_array |> List.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
