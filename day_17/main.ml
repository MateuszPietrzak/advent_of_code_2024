open Core

type computer = {
  mutable reg_a : int;
  mutable reg_b : int;
  mutable reg_c : int;
  program : int array;
}

let print_computer c =
  printf "=========\n";
  printf "%d %d %d\n" c.reg_a c.reg_b c.reg_c;
  Array.iter c.program ~f:(fun x -> printf "%d " x);
  printf "\n=========\n\n"

let get_combo computer = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> computer.reg_a
  | 5 -> computer.reg_b
  | 6 -> computer.reg_c
  | _ -> assert false

let get_res computer =
  let pc = ref 0 in
  let program_length = Array.length computer.program in
  let res = ref [] in
  while !pc < program_length do
    let opcode = computer.program.(!pc) in
    let operand = computer.program.(!pc + 1) in
    (match opcode with
    | 0 ->
        (* adv *)
        let num = computer.reg_a in
        let den = get_combo computer operand in
        let res = num lsr den in
        computer.reg_a <- res;
        pc := !pc + 2
    | 1 ->
        (* bxl *)
        computer.reg_b <- Int.bit_xor computer.reg_b operand;
        pc := !pc + 2
    | 2 ->
        (* bst *)
        computer.reg_b <- Int.bit_and (get_combo computer operand) 0b111;
        pc := !pc + 2
    | 3 ->
        (* jnz *)
        if computer.reg_a <> 0 then pc := operand else pc := !pc + 2
    | 4 ->
        (* bxc *)
        computer.reg_b <- Int.bit_xor computer.reg_b computer.reg_c;
        pc := !pc + 2
    | 5 ->
        (* out *)
        res := !res @ [ Int.bit_and (get_combo computer operand) 7 ];
        pc := !pc + 2
    | 6 ->
        (* bdv *)
        let num = computer.reg_a in
        let den = get_combo computer operand in
        let res = num lsr den in
        computer.reg_b <- res;
        pc := !pc + 2
    | 7 ->
        (* cdv *)
        let num = computer.reg_a in
        let den = get_combo computer operand in
        let res = num lsr den in
        computer.reg_c <- res;
        pc := !pc + 2
    | _ -> assert false);
    ()
  done;
  !res

let solve_1 computer =
  print_computer computer;
  let res = get_res computer in
  let str_res = List.map res ~f:Int.to_string |> String.concat ~sep:"," in
  printf "%s \n" str_res

let solve_2 computer =
  let program = List.of_array computer.program in
  let rec aux high program_rev =
    match program_rev with
    | [] -> Some high
    | p :: ps ->
        List.range 0 8
        |> List.fold ~init:None ~f:(fun acc x ->
               let reg_a = Int.(bit_or (high lsl 3) x) in
               match acc with
               | None ->
                   let calc =
                     Int.(
                       bit_and
                         (bit_xor (bit_xor reg_a 6) (reg_a lsr bit_xor x 3))
                         7)
                   in
                   if calc = p then aux reg_a ps else None
               | Some x -> Some x)
  in
  let res = aux 0 (List.rev program) in
  match res with None -> printf "Not found...\n" | Some x -> printf "%d\n" x

let parse_input lines =
  let open Re in
  let combined = String.concat ~sep:" " lines in
  let reg_a = seq [ str "Register A: "; group (rep1 digit) ] in
  let reg_b = seq [ str "Register B: "; group (rep1 digit) ] in
  let reg_c = seq [ str "Register C: "; group (rep1 digit) ] in
  let program =
    seq [ str "Program: "; group @@ rep1 @@ alt [ digit; char ',' ] ]
  in
  {
    reg_a = Group.get (exec (compile reg_a) combined) 1 |> Int.of_string;
    reg_b = Group.get (exec (compile reg_b) combined) 1 |> Int.of_string;
    reg_c = Group.get (exec (compile reg_c) combined) 1 |> Int.of_string;
    program =
      Group.get (exec (compile program) combined) 1
      |> String.split ~on:',' |> List.map ~f:Int.of_string |> Array.of_list;
  }

let () = Aoc.run_day solve_1 solve_2 parse_input
