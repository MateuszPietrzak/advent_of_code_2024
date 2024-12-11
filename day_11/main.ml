open Core

let memo = Array.init 76 ~f:(fun _ -> Hashtbl.create (module Int))

let rec get_num_stones blinks num =
  match Hashtbl.find memo.(blinks) num with
  | None ->
      let len =
        if num = 0 then 1 else Int.of_float (Float.log10 (Int.to_float num)) + 1
      in
      let res =
        if blinks = 0 then 1
        else if num = 0 then get_num_stones (blinks - 1) 1
        else if len % 2 = 0 then
          let pow = Int.pow 10 (len / 2) in
          get_num_stones (blinks - 1) (num / pow)
          + get_num_stones (blinks - 1) (num % pow)
        else get_num_stones (blinks - 1) (num * 2024)
      in
      Hashtbl.set memo.(blinks) ~key:num ~data:res;
      res
  | Some x -> x

let solve_1 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc x -> acc + get_num_stones 25 x)
  in
  printf "%d\n" res

let solve_2 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc x -> acc + get_num_stones 75 x)
  in
  printf "%d\n" res

let parse_input lines = List.hd_exn lines |> Aoc.int_list_of_line
let () = Aoc.run_day solve_1 solve_2 parse_input
