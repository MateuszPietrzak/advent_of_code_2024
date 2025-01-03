open Core

let solve_1 (a, b) =
  let a_sorted = List.sort a ~compare:Int.descending in
  let b_sorted = List.sort b ~compare:Int.descending in
  let combined = List.zip_exn a_sorted b_sorted in
  let result =
    List.map combined ~f:(fun (a, b) -> abs (a - b))
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" result

let solve_2 (a, b) =
  let map =
    List.fold b
      ~init:(Map.empty (module Int))
      ~f:(fun map x ->
        let new_count =
          match Map.find map x with None -> 1 | Some v -> v + 1
        in
        Map.set map ~key:x ~data:new_count)
  in
  let result =
    List.fold a ~init:0 ~f:(fun acc k ->
        let v = match Map.find map k with None -> 0 | Some v -> v in
        acc + (k * v))
  in
  printf "%d\n" result

let parse_input lines =
  lines
  |> List.map ~f:Aoc.int_list_of_line
  |> List.concat
  |> List.fold ~init:([], []) ~f:(fun (a, b) x ->
         if List.length a > List.length b then (a, x :: b) else (x :: a, b))

let () =
  Aoc.run_day solve_1 solve_2 parse_input 
