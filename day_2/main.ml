open Core

let rec line_safe_1 operator = function
  | [] -> true
  | [ _ ] -> true
  | [ x; y ] -> operator x y
  | x :: y :: xs -> operator x y && line_safe_1 operator (y :: xs)

let line_safe_2 operator input =
  let rec aux already_skipped = function
    | [] -> true
    | [ _ ] -> true
    | [ x; y ] -> operator x y || not already_skipped
    | [ x; y; z ] ->
        (operator x y && operator y z)
        || (not already_skipped && (operator x y || operator x z))
    | x :: y :: z :: xs ->
        (operator x y && aux already_skipped (y :: z :: xs))
        || (not already_skipped && operator x z && aux true (z :: xs))
  in
  aux false input
  || match List.tl input with None -> true | Some tl -> aux true tl

let get_result func input =
  let safe_inc =
    List.map input ~f:(fun line -> func (fun x y -> x < y && y - x <= 3) line)
  in
  let safe_dec =
    List.map input ~f:(fun line -> func (fun x y -> x > y && x - y <= 3) line)
  in
  let combined =
    List.zip_exn safe_inc safe_dec |> List.map ~f:(fun (a, b) -> a || b)
  in
  List.fold combined ~init:0 ~f:(fun acc x -> if x then acc + 1 else acc)

let solve_1 input =
  let result = get_result line_safe_1 input in
  printf "%d\n" result

let solve_2 input =
  let result = get_result line_safe_2 input in
  printf "%d\n" result

let parse_input lines = 
  lines |> List.map ~f:Aoc.int_list_of_line

let () =
  Aoc.run_day solve_1 solve_2 parse_input
