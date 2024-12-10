open Core

type point = { x : int; y : int }

let dfs input x y = 
  let height = Array.length input in
  let width = Array.length input.(0) in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let odw = Array.init height ~f:(fun _ -> Array.init width ~f:(const false)) in
  let res = ref 0 in
  let rec aux x y v = 
    if not @@ odw.(y).(x) then (
    odw.(y).(x) <- true;
    if v = 9 then Int.incr res;
    if is_safe (x - 1) y && input.(y).(x - 1) = v + 1 then aux (x - 1) y (v + 1);
    if is_safe (x + 1) y && input.(y).(x + 1) = v + 1 then aux (x + 1) y (v + 1);
    if is_safe x (y - 1) && input.(y - 1).(x) = v + 1 then aux x (y - 1) (v + 1);
    if is_safe x (y + 1) && input.(y + 1).(x) = v + 1 then aux x (y + 1) (v + 1)
    )
  in
  aux x y 0;
  !res

let solve_1 input =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if input.(i).(j) = 0 then res := !res + dfs input j i
    done
  done;
  printf "%d\n" !res


let propagate input scores queue elevation =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let new_q = Queue.create () in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  while not @@ Queue.is_empty queue do
    let { x; y } = Queue.dequeue_exn queue in
    if is_safe (x - 1) y && input.(y).(x - 1) = elevation then (
      if scores.(y).(x - 1) = 0 then Queue.enqueue new_q { x = x - 1; y };
      scores.(y).(x - 1) <- scores.(y).(x - 1) + scores.(y).(x));
    if is_safe (x + 1) y && input.(y).(x + 1) = elevation then (
      if scores.(y).(x + 1) = 0 then Queue.enqueue new_q { x = x + 1; y };
      scores.(y).(x + 1) <- scores.(y).(x + 1) + scores.(y).(x));
    if is_safe x (y - 1) && input.(y - 1).(x) = elevation then (
      if scores.(y - 1).(x) = 0 then Queue.enqueue new_q { x; y = y - 1 };
      scores.(y - 1).(x) <- scores.(y - 1).(x) + scores.(y).(x));
    if is_safe x (y + 1) && input.(y + 1).(x) = elevation then (
      if scores.(y + 1).(x) = 0 then Queue.enqueue new_q { x; y = y + 1 };
      scores.(y + 1).(x) <- scores.(y + 1).(x) + scores.(y).(x))
  done;
  while not @@ Queue.is_empty new_q do
    Queue.enqueue queue (Queue.dequeue_exn new_q)
  done

let solve_2 input =
  let q = Queue.create () in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let scores = Array.init height ~f:(fun _ -> Array.init width ~f:(const 0)) in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if input.(i).(j) = 9 then (
        Queue.enqueue q { x = j; y = i };
        scores.(i).(j) <- 1)
    done
  done;
  List.iter [ 8; 7; 6; 5; 4; 3; 2; 1; 0 ] ~f:(fun x ->
      propagate input scores q x);
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if input.(i).(j) = 0 then res := !res + scores.(i).(j)
    done;
  done;
  printf "%d\n" !res

let parse_input lines =
  List.to_array lines
  |> Array.map ~f:(fun s ->
         String.to_array s
         |> Array.map ~f:(fun e -> Char.to_int e - Char.to_int '0'))

let () = Aoc.run_day solve_1 solve_2 parse_input
