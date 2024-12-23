open! Core

let code_to_index a b =
  let beg = Char.to_int 'a' in
  Char.(((to_int a - beg) * 31) + to_int b - beg)

let index_starts_with num c =
  let v1 = Char.to_int c - Char.to_int 'a' and v2 = num / 31 in
  v1 = v2

let edge_list_to_adj_list list =
  let res = Array.init 1024 ~f:(fun _ -> []) in
  List.iter list ~f:(fun (a, b) ->
      res.(a) <- b :: res.(a);
      res.(b) <- a :: res.(b));
  res

let solve_1 input =
  let adj_list = edge_list_to_adj_list input in
  let set = ref @@ Set.empty (module Int) in
  let check v1 v2 =
    match List.find adj_list.(v1) ~f:(( = ) v2) with
    | None -> false
    | Some _ -> true
  in
  Array.iteri adj_list ~f:(fun i list ->
      if List.length list > 0 && index_starts_with i 't' then
        List.iter (List.cartesian_product list list) ~f:(fun (v1, v2) ->
            if v1 <> v2 && check v1 v2 then
              let l = List.sort [ v1; v2; i ] ~compare:Int.compare in
              match l with
              | [ a; b; c ] ->
                  set := Set.add !set ((a * 1048576) + (b * 1024) + c)
              | _ -> assert false);
      ());
  printf "%d\n" (Set.length !set)

let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let combined = String.concat ~sep:" " lines in
  let exp = seq [ group (rep1 alpha); char '-'; group (rep1 alpha) ] in
  all (compile exp) combined
  |> List.map ~f:(fun g ->
         let g1 = Group.get g 1 and g2 = Group.get g 2 in
         ( code_to_index (String.get g1 0) (String.get g1 1),
           code_to_index (String.get g2 0) (String.get g2 1) ))

let () = Aoc.run_day solve_1 solve_2 parse_input
