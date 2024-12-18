open Core

type point = { x : int; y : int }

let get_arr ~input ~time =
  let width = 71 in
  let height = 71 in
  let arr = Array.init height ~f:(fun _ -> Array.init width ~f:(const 0)) in
  arr.(0).(0) <- 1;
  List.take input time |> List.iter ~f:(fun p -> arr.(p.y).(p.x) <- -1);
  let q = Queue.singleton { x = 0; y = 0 } in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  while not @@ Queue.is_empty q do
    let p = Queue.dequeue_exn q in
    let dist = arr.(p.y).(p.x) in
    if is_safe (p.x + 1) p.y && arr.(p.y).(p.x + 1) = 0 then (
      arr.(p.y).(p.x + 1) <- dist + 1;
      Queue.enqueue q { x = p.x + 1; y = p.y });
    if is_safe (p.x - 1) p.y && arr.(p.y).(p.x - 1) = 0 then (
      arr.(p.y).(p.x - 1) <- dist + 1;
      Queue.enqueue q { x = p.x - 1; y = p.y });
    if is_safe p.x (p.y + 1) && arr.(p.y + 1).(p.x) = 0 then (
      arr.(p.y + 1).(p.x) <- dist + 1;
      Queue.enqueue q { x = p.x; y = p.y + 1 });
    if is_safe p.x (p.y - 1) && arr.(p.y - 1).(p.x) = 0 then (
      arr.(p.y - 1).(p.x) <- dist + 1;
      Queue.enqueue q { x = p.x; y = p.y - 1 })
  done;
  arr.(height - 1).(width - 1) - 1

let solve_1 input =
  printf "%d\n" (get_arr ~input ~time:1024);
  ()

let solve_2 input =
  let rec aux time =
    let res = get_arr ~input ~time in
    if res > 0 then aux (time + 1)
    else
      let _, p = List.findi_exn input ~f:(fun i _ -> i = time - 1) in
      p
  in
  let p = aux 0 in
  printf "%d %d\n" p.x p.y

let parse_input lines =
  List.map lines ~f:(fun s -> String.split s ~on:',')
  |> List.map ~f:(function
       | [ a; b ] -> { x = Int.of_string a; y = Int.of_string b }
       | _ -> assert false)

let () = Aoc.run_day solve_1 solve_2 parse_input
