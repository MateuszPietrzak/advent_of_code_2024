open! Core

type position = { mutable x : int; mutable y : int }

let solve_1 input =
  let height = Array.length input and width = Array.length input.(0) in
  let start_pos = { x = 0; y = 0 } and end_pos = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = 'S') then (
        input.(i).(j) <- '.';
        start_pos.x <- j;
        start_pos.y <- i);
      if Char.(input.(i).(j) = 'E') then (
        input.(i).(j) <- '.';
        end_pos.x <- j;
        end_pos.y <- i)
    done
  done;
  let q = Queue.singleton start_pos in
  let dist =
    Array.init height ~f:(fun _ -> Array.init width ~f:(const Int.max_value))
  in
  dist.(start_pos.y).(start_pos.x) <- 0;
  let deltas =
    [ { x = -1; y = 0 }; { x = 1; y = 0 }; { x = 0; y = -1 }; { x = 0; y = 1 } ]
  in
  while not @@ Queue.is_empty q do
    let p = Queue.dequeue_exn q in
    List.iter deltas ~f:(fun { x = dx; y = dy } ->
        if
          Char.(input.(p.y + dy).(p.x + dx) = '.')
          && dist.(p.y + dy).(p.x + dx) = Int.max_value
        then (
          dist.(p.y + dy).(p.x + dx) <- dist.(p.y).(p.x) + 1;
          Queue.enqueue q { x = p.x + dx; y = p.y + dy }))
  done;
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = '.') && dist.(i).(j) <> Int.max_value then
        let p = { x = j; y = i } in
        List.iter deltas ~f:(fun { x = dx; y = dy } ->
            if Char.(input.(p.y + dy).(p.x + dx) = '#') then
              List.iter deltas ~f:(fun { x = dx2; y = dy2 } ->
                  if
                    is_safe (p.x + dx + dx2) (p.y + dy + dy2)
                    && Char.(input.(p.y + dy + dy2).(p.x + dx + dx2) = '.')
                    && dist.(p.y + dy + dy2).(p.x + dx + dx2) <> Int.max_value
                    && dist.(p.y + dy + dy2).(p.x + dx + dx2)
                       - dist.(p.y).(p.x)
                       - 2
                       >= 100
                  then Int.incr res))
    done
  done;
  printf "%d\n" !res

let get_score input dist x y =
  let height = Array.length input and width = Array.length input.(0) in
  let arr =
    Array.init height ~f:(fun _ -> Array.init width ~f:(const Int.max_value))
  in
  arr.(y).(x) <- 0;
  let q = Queue.singleton { x; y } in
  let res = ref 0 in
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let deltas =
    [ { x = -1; y = 0 }; { x = 1; y = 0 }; { x = 0; y = -1 }; { x = 0; y = 1 } ]
  in
  while not @@ Queue.is_empty q do
    let p = Queue.dequeue_exn q in
    if
      Char.(input.(p.y).(p.x) = '.')
      && dist.(p.y).(p.x) - dist.(y).(x) - arr.(p.y).(p.x) >= 100
    then (
      Int.incr res);

    List.iter deltas ~f:(fun { x = dx; y = dy } ->
        if
          is_safe (p.x + dx) (p.y + dy)
          && arr.(p.y).(p.x) < 20
          && arr.(p.y + dy).(p.x + dx) = Int.max_value
        then (
          arr.(p.y + dy).(p.x + dx) <- arr.(p.y).(p.x) + 1;
          Queue.enqueue q { x = p.x + dx; y = p.y + dy }))
  done;
  !res

let solve_2 input =
  let height = Array.length input and width = Array.length input.(0) in
  let start_pos = { x = 0; y = 0 } and end_pos = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = 'S') then (
        input.(i).(j) <- '.';
        start_pos.x <- j;
        start_pos.y <- i);
      if Char.(input.(i).(j) = 'E') then (
        input.(i).(j) <- '.';
        end_pos.x <- j;
        end_pos.y <- i)
    done
  done;
  let q = Queue.singleton start_pos in
  let dist =
    Array.init height ~f:(fun _ -> Array.init width ~f:(const Int.max_value))
  in
  dist.(start_pos.y).(start_pos.x) <- 0;
  let deltas =
    [ { x = -1; y = 0 }; { x = 1; y = 0 }; { x = 0; y = -1 }; { x = 0; y = 1 } ]
  in
  while not @@ Queue.is_empty q do
    let p = Queue.dequeue_exn q in
    List.iter deltas ~f:(fun { x = dx; y = dy } ->
        if
          Char.(input.(p.y + dy).(p.x + dx) = '.')
          && dist.(p.y + dy).(p.x + dx) = Int.max_value
        then (
          dist.(p.y + dy).(p.x + dx) <- dist.(p.y).(p.x) + 1;
          Queue.enqueue q { x = p.x + dx; y = p.y + dy }))
  done;
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(input.(i).(j) = '.') && dist.(i).(j) <> Int.max_value then
        res := !res + get_score input dist j i
    done
  done;
  printf "%d\n" !res

let parse_input lines = List.map lines ~f:String.to_array |> List.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
