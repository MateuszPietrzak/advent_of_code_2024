open! Core

type gate = And | Or | Xor

type connection = {
  input_1 : string;
  input_2 : string;
  gate : gate;
  output : string;
}

type wire = { label : string; state : bool }

let gate_function = function
  | And -> ( && )
  | Or -> ( || )
  | Xor -> fun a b -> Bool.(a <> b)

let hash_label label =
  let chars = String.to_list label in
  let a_code = Char.to_int 'a' - 10 and zero_code = Char.to_int '0' in
  let rec aux acc = function
    | x :: xs ->
        if Char.is_alpha x then aux ((acc * 37) + (Char.to_int x - a_code)) xs
        else aux ((acc * 37) + (Char.to_int x - zero_code)) xs
    | [] -> acc
  in
  aux 0 chars

let organize_edge_list conns =
  let back_connections = Array.init 50653 ~f:(const None) in
  let forward_connections = Array.init 50653 ~f:(const None) in
  List.iter conns ~f:(fun con ->
      let output_hash = hash_label con.output
      and input_1_hash = hash_label con.input_1
      and input_2_hash = hash_label con.input_2 in
      printf "%d %s\n" input_2_hash con.input_2;
      back_connections.(output_hash) <- Some con;
      forward_connections.(input_1_hash) <-
        (match forward_connections.(input_1_hash) with
        | None -> Some [ output_hash ]
        | Some xs -> Some (output_hash :: xs));
      forward_connections.(input_2_hash) <-
        (match forward_connections.(input_2_hash) with
        | None -> Some [ output_hash ]
        | Some xs -> Some (output_hash :: xs)));
  (back_connections, forward_connections)

let solve_1 (states, conns) =
  printf "%d\n" (hash_label "fgs");
  printf "======================\n";
  List.iter states ~f:(fun wire -> printf "%s %b\n" wire.label wire.state);
  printf "======================\n";
  List.iter conns ~f:(fun con ->
      printf "%s %s %s -> %s\n" con.input_1
        (match con.gate with And -> "And" | Or -> "Or" | Xor -> "Xor")
        con.input_2 con.output);
  printf "======================\n";

  let back_conns, forward_conns = organize_edge_list conns in
  let node_degree = Array.init 50653 ~f:(const 0) in
  let outputs = Array.init 50653 ~f:(const false) in
  let q = Queue.create () in

  List.iter conns ~f:(fun con ->
      node_degree.(hash_label con.input_1) <- 2;
      node_degree.(hash_label con.input_2) <- 2;
      node_degree.(hash_label con.output) <- 2);
  List.iter states ~f:(fun con ->
      let hash = hash_label con.label in
      outputs.(hash) <- con.state;
      match forward_conns.(hash) with
      | None -> ()
      | Some l ->
          List.iter l ~f:(fun x ->
              node_degree.(x) <- node_degree.(x) - 1;
              if node_degree.(x) = 0 then Queue.enqueue q x));

  while not @@ Queue.is_empty q do
    let ind = Queue.dequeue_exn q in
    let con =
      match back_conns.(ind) with None -> assert false | Some x -> x
    in
    let operator = gate_function con.gate in
    outputs.(ind) <-
      operator outputs.(hash_label con.input_1) outputs.(hash_label con.input_2);
    match forward_conns.(ind) with
    | None -> ()
    | Some l ->
        List.iter l ~f:(fun x ->
            node_degree.(x) <- node_degree.(x) - 1;
            if node_degree.(x) = 0 then Queue.enqueue q x)
  done;
  let res =
    List.range (hash_label "z00") (hash_label "z09" + 1)
    @ List.range (hash_label "z10") (hash_label "z19" + 1)
    @ List.range (hash_label "z20") (hash_label "z29" + 1)
    @ List.range (hash_label "z30") (hash_label "z39" + 1)
    @ List.range (hash_label "z40") (hash_label "z49" + 1)
    |> List.rev
    |> List.fold ~init:0 ~f:(fun acc x ->
           printf "%d" (if outputs.(x) then 1 else 0);
           (2 * acc) + if outputs.(x) then 1 else 0)
  in

  printf "\n";
  printf "%d\n" res

let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let open List.Monad_infix in
  let wire_state_exp =
    seq [ group (rep1 alnum); str ": "; group (alt [ char '0'; char '1' ]) ]
  and wire_conns_exp =
    seq
      [
        group (rep1 alnum);
        space;
        group (alt [ str "AND"; str "XOR"; str "OR" ]);
        space;
        group (rep1 alnum);
        str " -> ";
        group (rep1 alnum);
      ]
  and combined_lines = String.concat ~sep:", " lines in
  let states =
    all (compile wire_state_exp) combined_lines >>| fun g ->
    let label = Group.get g 1
    and bit =
      match Group.get g 2 with "0" -> false | "1" -> true | _ -> assert false
    in
    { label; state = bit }
  and conns =
    all (compile wire_conns_exp) combined_lines >>| fun g ->
    let label_1 = Group.get g 1
    and gate =
      match Group.get g 2 with
      | "AND" -> And
      | "OR" -> Or
      | "XOR" -> Xor
      | _ -> assert false
    and label_2 = Group.get g 3
    and label_3 = Group.get g 4 in
    { input_1 = label_1; input_2 = label_2; gate; output = label_3 }
  in
  (states, conns)

let () = Aoc.run_day solve_1 solve_2 parse_input
