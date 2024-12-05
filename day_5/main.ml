open Core

let is_correct rules ordering =
  let rules_arr = Array.init 100 ~f:(fun _ -> Set.empty (module Int)) in
  List.iter rules ~f:(fun (a, b) -> rules_arr.(a) <- Set.add rules_arr.(a) b);
  let ordering_arr = Array.of_list ordering in
  let cur_set = ref @@ Set.empty (module Int) in
  let is_correct = ref true in
  for i = 0 to Array.length ordering_arr - 1 do
    let inter = Set.inter !cur_set rules_arr.(ordering_arr.(i)) in
    if Set.length inter > 0 then is_correct := false else ();
    cur_set := Set.add !cur_set ordering_arr.(i);
    ()
  done;
  !is_correct

let get_middle_if_correct rules ordering =
  let ordering_arr = Array.of_list ordering in
  let is_correct = is_correct rules ordering in
  if not is_correct then 0 else ordering_arr.(Array.length ordering_arr / 2)

let solve_1 (rules, orderings) =
  let result =
    List.map orderings ~f:(fun o -> get_middle_if_correct rules o)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" result

let topo_sort rules ordering =
  let vertices =
    List.fold ordering
      ~init:(Set.empty (module Int))
      ~f:(fun acc x -> Set.add acc x)
  in
  let incoming_edges = Array.init 100 ~f:(const 0) in
  List.iter rules ~f:(fun (a, b) ->
      if Set.mem vertices a && Set.mem vertices b then
      incoming_edges.(b) <- incoming_edges.(b) + 1);
  let edges = Array.init 100 ~f:(fun _ -> Set.empty (module Int)) in
  List.iter rules ~f:(fun (a, b) ->
      if Set.mem vertices a && Set.mem vertices b then
        edges.(a) <- Set.add edges.(a) b);
  let to_consider = Queue.create () in
  Set.iter vertices ~f:(fun v ->
      if equal incoming_edges.(v) 0 then
        Queue.enqueue to_consider v);
  let result = Dynarray.create () in
  while not @@ Queue.is_empty to_consider do
    let front = Queue.dequeue_exn to_consider in
    Dynarray.add_last result front;
    Set.iter edges.(front) ~f:(fun v -> 
      incoming_edges.(v) <- incoming_edges.(v) - 1;
      if equal incoming_edges.(v) 0 then
        Queue.enqueue to_consider v
      );
  done;
  Dynarray.to_list result

let solve_2 (rules, orderings) =
  let incorrect =
    List.filter orderings ~f:(fun o -> not @@ is_correct rules o)
  in
  let toposorted = List.map incorrect ~f:(fun o -> topo_sort rules o) in
  let result =
    List.fold toposorted ~init:0 ~f:(fun acc l ->
        let arr = List.to_array l in
        acc + arr.(Array.length arr / 2))
  in
  printf "%d\n" result

let parse_input lines =
  let open Re in
  let exp_1 = seq [ group (rep1 digit); str "|"; group (rep1 digit) ] in
  let exp_2 =
    group @@ seq [ rep1 @@ seq [ rep1 digit; str "," ]; rep1 digit ]
  in
  let re_1 = compile exp_1 in
  let re_2 = compile exp_2 in
  let combined_input = String.concat ~sep:" " lines in
  let matches_rules = all re_1 combined_input in
  let matches_ordering = all re_2 combined_input in
  let rules =
    matches_rules
    |> List.map ~f:(fun g ->
           (Int.of_string @@ Group.get g 1, Int.of_string @@ Group.get g 2))
  in
  let orderings =
    matches_ordering
    |> List.map ~f:(fun g -> Group.get g 1)
    |> List.map ~f:(fun str ->
           String.split ~on:',' str |> List.map ~f:Int.of_string)
  in
  (rules, orderings)

let () = Aoc.run_day solve_1 solve_2 parse_input
