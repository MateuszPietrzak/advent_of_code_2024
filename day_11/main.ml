open Core

let memo = Array.init 76 ~f:(fun _ -> Hashtbl.create (module Int))

let rec get_num_stones blinks num =
  match Hashtbl.find memo.(blinks) num with
  | None ->
      let res =
        if blinks = 0 then 1
        else if num = 0 then get_num_stones (blinks - 1) 1
        else if String.length (Int.to_string num) % 2 = 0 then
          let s = Int.to_string num in
          let len = String.length s in
          let s1 = String.drop_prefix s (len / 2) in
          let s2 = String.drop_suffix s (len / 2) in
          get_num_stones (blinks - 1) (Int.of_string s1)
          + get_num_stones (blinks - 1) (Int.of_string s2)
        else get_num_stones (blinks - 1) (num * 2024)
      in
      Hashtbl.set memo.(blinks) ~key:num ~data:res;
      res
  | Some x -> x

let solve_1 input =
  List.iter input ~f:(fun x -> printf "%d " x);
  printf "\n";
  let res =
    List.fold input ~init:0 ~f:(fun acc x -> acc + get_num_stones 25 x)
  in
  printf "%d\n" res

let solve_2 input =
  List.iter input ~f:(fun x -> printf "%d " x);
  printf "\n";
  let res =
    List.fold input ~init:0 ~f:(fun acc x -> acc + get_num_stones 75 x)
  in
  printf "%d\n" res

let parse_input lines = List.hd_exn lines |> Aoc.int_list_of_line
let () = Aoc.run_day solve_1 solve_2 parse_input
