open! Core

let solve_1 (locks, keys) =
  let res = ref 0 in
  List.iter (List.cartesian_product locks keys) ~f:(fun (lock, key) ->
      if
        List.fold2_exn lock key ~init:true ~f:(fun acc l k -> acc && l + k < 6)
      then Int.incr res);
  printf "%d\n" !res;
  ()

let solve_2 _input = ()

let parse_input lines =
  let open List.Monad_infix in
  let grouped = List.groupi lines ~break:(fun i _ _ -> i mod 8 = 0) in
  List.fold grouped ~init:([], []) ~f:(fun (lock_acc, key_acc) group ->
      let g =
        if List.length group = 8 then List.drop_last_exn group else group
      in
      match List.hd_exn g with
      | "#####" ->
          let l =
            List.transpose_exn @@ List.tl_exn (g >>| String.to_list) |> List.rev
          in
          let code =
            List.fold l ~init:[] ~f:(fun acc x ->
                let prefix = List.take_while x ~f:(Char.( = ) '#') in
                List.length prefix :: acc)
          in
          (code :: lock_acc, key_acc)
      | "....." ->
          let l =
            List.transpose_exn @@ List.tl_exn @@ List.rev (g >>| String.to_list)
            >>| List.rev |> List.rev
          in
          let code =
            List.fold l ~init:[] ~f:(fun acc x ->
                let prefix =
                  List.rev x |> List.take_while ~f:(Char.( = ) '#')
                in
                List.length prefix :: acc)
          in
          (lock_acc, code :: key_acc)
      | _ -> assert false)

let () = Aoc.run_day solve_1 solve_2 parse_input
