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

let move_get_new_pos_2 board pos move =
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
  else if
    Char.(
      board.(pos.y + d.y).(pos.x + d.x) = '['
      || board.(pos.y + d.y).(pos.x + d.x) = ']')
  then (
    match move with
    | Right | Left -> (
        let f_pos = { x = pos.x + d.x; y = pos.y } in
        while
          Char.(
            board.(f_pos.y).(f_pos.x) = '[' || board.(f_pos.y).(f_pos.x) = ']')
        do
          f_pos.x <- f_pos.x + d.x
        done;
        match board.(f_pos.y).(f_pos.x) with
        | '#' -> pos
        | '.' ->
            while f_pos.x <> pos.x + d.x do
              board.(f_pos.y).(f_pos.x) <- board.(f_pos.y).(f_pos.x - d.x);
              f_pos.x <- f_pos.x - d.x
            done;
            board.(f_pos.y).(f_pos.x) <- '.';
            { x = pos.x + d.x; y = pos.y + d.y }
        | _ -> assert false)
    | Up | Down ->
        let q = Queue.create () in
        Queue.enqueue q
          {
            x =
              (pos.x - if Char.(board.(pos.y + d.y).(pos.x) = ']') then 1 else 0);
            y = pos.y + d.y;
          };
        let found_wall = ref false in
        let stack_to_move = Stack.create () in
        while not @@ Queue.is_empty q do
          let f_pos = Queue.dequeue_exn q in
          Stack.push stack_to_move f_pos;
          if
            Char.(
              board.(f_pos.y + d.y).(f_pos.x) = '#'
              || board.(f_pos.y + d.y).(f_pos.x + 1) = '#')
          then found_wall := true
          else if Char.(board.(f_pos.y + d.y).(f_pos.x) = '[') then
            Queue.enqueue q { x = f_pos.x; y = f_pos.y + d.y }
          else if
            Char.(
              board.(f_pos.y + d.y).(f_pos.x) = ']'
              && board.(f_pos.y + d.y).(f_pos.x + 1) = '[')
          then (
            Queue.enqueue q { x = f_pos.x - 1; y = f_pos.y + d.y };
            Queue.enqueue q { x = f_pos.x + 1; y = f_pos.y + d.y })
          else if Char.(board.(f_pos.y + d.y).(f_pos.x) = ']') then
            Queue.enqueue q { x = f_pos.x - 1; y = f_pos.y + d.y }
          else if Char.(board.(f_pos.y + d.y).(f_pos.x + 1) = '[') then
            Queue.enqueue q { x = f_pos.x + 1; y = f_pos.y + d.y }
        done;

        if !found_wall then pos
        else (
          while not @@ Stack.is_empty stack_to_move do
            let f_pos = Stack.pop_exn stack_to_move in
            board.(f_pos.y).(f_pos.x) <- '.';
            board.(f_pos.y).(f_pos.x + 1) <- '.';
            board.(f_pos.y + d.y).(f_pos.x) <- '[';
            board.(f_pos.y + d.y).(f_pos.x + 1) <- ']';
            ()
          done;
          { x = pos.x; y = pos.y + d.y }))
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
  List.iter moves ~f:(fun m ->
      let new_pos = move_get_new_pos board pos m in
      pos.x <- new_pos.x;
      pos.y <- new_pos.y);
  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(board.(i).(j) = 'O') then res := !res + (100 * i) + j
    done
  done;
  printf "%d\n" !res

let solve_2 (board, moves) =
  let wide_board =
    Array.map board ~f:(fun row ->
        let new_row = Array.init (Array.length row * 2) ~f:(const '.') in
        Array.iteri row ~f:(fun i c ->
            let c1, c2 =
              match c with
              | '.' -> ('.', '.')
              | 'O' -> ('[', ']')
              | '#' -> ('#', '#')
              | '@' -> ('@', '.')
              | _ -> assert false
            in
            new_row.(2 * i) <- c1;
            new_row.((2 * i) + 1) <- c2;
            ());
        new_row)
  in

  let height = Array.length wide_board in
  let width = Array.length wide_board.(0) in
  let pos = { x = 0; y = 0 } in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(wide_board.(i).(j) = '@') then (
        pos.x <- j;
        pos.y <- i;
        wide_board.(i).(j) <- '.')
    done
  done;

  List.iter moves ~f:(fun m ->
      let new_pos = move_get_new_pos_2 wide_board pos m in
      pos.x <- new_pos.x;
      pos.y <- new_pos.y);

  let res = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if Char.(wide_board.(i).(j) = '[') then res := !res + (100 * i) + j
    done
  done;
  printf "%d\n" !res

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
