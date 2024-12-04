open Core

let count_horiz_line line =
  let open Re in
  let exp_1 = str "XMAS" in
  let re_1 = compile exp_1 in
  let exp_2 = str "SAMX" in
  let re_2 = compile exp_2 in
  let m = matches re_1 line @ matches re_2 line in
  List.length m


let count_diag input =
  let height = Array.length input in
  let width = Array.length input.(0) in
  let n = width + height in
  let res = ref 0 in
  for i = 0 to n do
    let y = ref i in
    let x = ref 0 in
    let arr = Array.init n ~f:(const '.') in
    while !x <> width && !x >= 0 do
      if !x < width && !x >= 0 && !y < height && !y >= 0 then 
        arr.(!x) <- input.(!y).(!x)
      else ();
      Int.incr x;
      Int.decr y;
    done;
    let str = String.of_array arr in
    printf "%s\n" str;
    let to_add = count_horiz_line str in
    res := !res + to_add;
  done;
  !res

let solve_1 input =
  let horizontal_result =
    List.map input ~f:count_horiz_line |> List.fold ~init:0 ~f:( + )
  in
  let transposed_input =
    List.map input ~f:String.to_list
    |> List.transpose_exn |> List.map ~f:String.of_list
  in
  let vertical_result =
    List.map transposed_input ~f:count_horiz_line |> List.fold ~init:0 ~f:( + )
  in
  let char_matrix =
    List.map input ~f:String.to_list
    |> Array.of_list |> Array.map ~f:Array.of_list
  in
  let diag_1_result = count_diag char_matrix in
  printf "%d\n" @@ horizontal_result + vertical_result + diag_1_result

let solve_2 _input = ()
let parse_input lines = lines
let () = Aoc.run_day solve_1 solve_2 parse_input
