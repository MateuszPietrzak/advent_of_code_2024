open! Core

let mix x y = Int.bit_xor x y
let prune x = x % 16777216

let solve_1 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc x ->
        let rec aux num depth =
          match depth with
          | 0 -> num
          | d ->
              let v1 = prune @@ mix (num * 64) num in
              let v2 = prune @@ mix (v1 / 32) v1 in
              let v3 = prune @@ mix (v2 * 2048) v2 in
              aux v3 (d - 1)
        in
        let res = aux x 2000 in
        acc + res)
  in
  printf "%d\n" res

module Pattern = struct
  module T = struct
    type t = int * int * int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparator.Make (T)
end

let get_list_digits num =
  let rec aux num depth =
    match depth with
    | 0 -> []
    | d ->
        let v1 = Int.(bit_and (bit_xor (num lsl 6) num) 16777215) in
        let v2 = Int.(bit_and (bit_xor (v1 lsr 5) v1) 16777215) in
        let v3 = Int.(bit_and (bit_xor (v2 lsl 11) v2) 16777215) in
        v3 :: aux v3 (d - 1)
  in
  num :: aux num 2000 |> List.map ~f:(fun x -> x % 10)

let rec get_diffs = function
  | [] -> []
  | x :: y :: xs -> (y - x) :: get_diffs (y :: xs)
  | _ -> []

let solve_2 input =
  let combined = Hashtbl.create (module Pattern) in
  List.iter input ~f:(fun x ->
      let digits = get_list_digits x in
      let diffs = get_diffs digits in
      let tbl = Hashtbl.create (module Pattern) in
      let rec accumulate dig dif =
        match dif with
        | a :: b :: c :: d :: xs ->
            let _ = Hashtbl.add tbl ~key:(a, b, c, d) ~data:(List.hd_exn dig) in
            accumulate (List.tl_exn dig) (b :: c :: d :: xs)
        | _ -> ()
      in
      accumulate (List.drop digits 4) diffs;
      Hashtbl.iter_keys tbl ~f:(fun pattern ->
          match Hashtbl.find combined pattern with
          | None ->
              Hashtbl.set combined ~key:pattern
                ~data:(Hashtbl.find_exn tbl pattern)
          | Some x ->
              Hashtbl.set combined ~key:pattern
                ~data:(x + Hashtbl.find_exn tbl pattern)));
  let res =
    Hashtbl.fold combined ~init:0 ~f:(fun ~key:_ ~data acc -> Int.max acc data)
  in
  printf "%d\n" res

let parse_input lines = lines |> List.map ~f:Int.of_string
let () = Aoc.run_day solve_1 solve_2 parse_input
