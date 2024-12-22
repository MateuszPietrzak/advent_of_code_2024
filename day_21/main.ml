open! Core

type point = { x : int; y : int }
type direction = Up | Right | Down | Left | Press

let ( -$ ) p1 p2 = { x = p1.x - p2.x; y = p1.y - p2.y }

let digit_to_pos = function
  | '7' -> { x = 0; y = 0 }
  | '8' -> { x = 1; y = 0 }
  | '9' -> { x = 2; y = 0 }
  | '4' -> { x = 0; y = 1 }
  | '5' -> { x = 1; y = 1 }
  | '6' -> { x = 2; y = 1 }
  | '1' -> { x = 0; y = 2 }
  | '2' -> { x = 1; y = 2 }
  | '3' -> { x = 2; y = 2 }
  | '0' -> { x = 1; y = 3 }
  | 'A' -> { x = 2; y = 3 }
  | _ -> assert false

let direction_to_pos = function
  | Up -> { x = 1; y = 0 }
  | Press -> { x = 2; y = 0 }
  | Left -> { x = 0; y = 1 }
  | Down -> { x = 1; y = 1 }
  | Right -> { x = 2; y = 1 }

let direction_to_index = function
  | Up -> 0
  | Press -> 1
  | Left -> 2
  | Down -> 3
  | Right -> 4

(* memo.(dir).(y).(x).(depth) *)
let memo =
  Array.init 5 ~f:(fun _ ->
      Array.init 2 ~f:(fun _ ->
          Array.init 3 ~f:(fun _ -> Array.init 30 ~f:(const None))))

let rec dir_to_score dir prev_pos depth =
  match memo.(direction_to_index dir).(prev_pos.y).(prev_pos.x).(depth) with
  | None ->
      let new_pos = direction_to_pos dir in
      let delta = new_pos -$ prev_pos in
      let horiz =
        if delta.x > 0 then List.init delta.x ~f:(const Right)
        else if delta.x < 0 then List.init (-delta.x) ~f:(const Left)
        else []
      in
      let vert =
        if delta.y > 0 then List.init delta.y ~f:(const Down)
        else if delta.y < 0 then List.init (-delta.y) ~f:(const Up)
        else []
      in
      let len1, _ =
        List.fold
          (vert @ horiz @ [ Press ])
          ~init:(0, { x = 2; y = 0 })
          ~f:(fun (acc, prev_pos) x ->
            let res, new_pos =
              if depth > 0 then dir_to_score x prev_pos (depth - 1)
              else (1, prev_pos)
            in
            (acc + res, new_pos))
      in
      let len2, _ =
        List.fold
          (horiz @ vert @ [ Press ])
          ~init:(0, { x = 2; y = 0 })
          ~f:(fun (acc, prev_pos) x ->
            let res, new_pos =
              if depth > 0 then dir_to_score x prev_pos (depth - 1)
              else (1, prev_pos)
            in
            (acc + res, new_pos))
      in

      let res =
        if prev_pos.x = 0 && new_pos.y = 0 then (len2, new_pos)
        else if prev_pos.y = 0 && new_pos.x = 0 then (len1, new_pos)
        else (Int.min len1 len2, new_pos)
      in
      memo.(direction_to_index dir).(prev_pos.y).(prev_pos.x).(depth) <-
        Some res;
      res
  | Some x -> x

let digit_to_score digit prev_pos depth =
  let new_pos = digit_to_pos digit in
  let delta = new_pos -$ prev_pos in
  let horiz =
    if delta.x > 0 then List.init delta.x ~f:(const Right)
    else if delta.x < 0 then List.init (-delta.x) ~f:(const Left)
    else []
  in
  let vert =
    if delta.y > 0 then List.init delta.y ~f:(const Down)
    else if delta.y < 0 then List.init (-delta.y) ~f:(const Up)
    else []
  in
  let len1, _ =
    List.fold
      (vert @ horiz @ [ Press ])
      ~init:(0, { x = 2; y = 0 })
      ~f:(fun (acc, prev_pos) x ->
        let res, new_pos = dir_to_score x prev_pos (depth - 1) in
        (acc + res, new_pos))
  in
  let len2, _ =
    List.fold
      (horiz @ vert @ [ Press ])
      ~init:(0, { x = 2; y = 0 })
      ~f:(fun (acc, prev_pos) x ->
        let res, new_pos = dir_to_score x prev_pos (depth - 1) in
        (acc + res, new_pos))
  in
  if prev_pos.x = 0 && new_pos.y = 3 then (len2, new_pos)
  else if prev_pos.y = 3 && new_pos.x = 0 then (len1, new_pos)
  else (Int.min len1 len2, new_pos)

let solve_1 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc digits ->
        let len, _ =
          List.fold digits
            ~init:(0, { x = 2; y = 3 })
            ~f:(fun (acc, prev_pos) x ->
              let res, new_pos = digit_to_score x prev_pos 2 in
              (acc + res, new_pos))
        and num =
          Int.of_string
            (String.chop_suffix_exn (String.of_list digits) ~suffix:"A")
        in
        acc + (len * num))
  in
  printf "%d\n" res

let solve_2 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc digits ->
        let len, _ =
          List.fold digits
            ~init:(0, { x = 2; y = 3 })
            ~f:(fun (acc, prev_pos) x ->
              let res, new_pos = digit_to_score x prev_pos 25 in
              (acc + res, new_pos))
        and num =
          Int.of_string
            (String.chop_suffix_exn (String.of_list digits) ~suffix:"A")
        in
        acc + (len * num))
  in
  printf "%d\n" res

let parse_input lines = List.map lines ~f:String.to_list
let () = Aoc.run_day solve_1 solve_2 parse_input
