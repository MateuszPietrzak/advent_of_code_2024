open Core

type vec = { x : int; y : int }
type robot = { p : vec; v : vec }
type quadrants = { tl : int; tr : int; bl : int; br : int }

let print_robot r = printf "p = (%d, %d) v = (%d, %d)\n" r.p.x r.p.y r.v.x r.v.y

let get_robot_pos p w h r = 
  let rec aux p x y =
    if p = 0 then {x = x; y = y} 
    else aux (p - 1) ((x + r.v.x + w) % w) ((y + r.v.y + h) % h) 
  in
  aux p r.p.x r.p.y

let solve_1 input =
  List.iter input ~f:print_robot;
  printf "========================\n";
  let width = 101 in
  let height = 103 in
  let mid_v = width / 2 in
  let mid_h = height / 2 in
  let quads = List.fold input ~init:{tl = 0; tr = 0; bl = 0; br = 0} ~f:(fun acc r ->
    let p = get_robot_pos 100 width height r in   
    if p.x > mid_v then
      if p.y > mid_h then
        {acc with br = acc.br + 1}
      else if p.y < mid_h then
        {acc with tr = acc.tr + 1}
      else acc
    else if p.x < mid_v then
      if p.y > mid_h then
        {acc with bl = acc.bl + 1}
      else if p.y < mid_h then
        {acc with tl = acc.tl + 1}
      else acc
    else acc
  ) in
  let res = quads.br * quads.bl * quads.tr * quads.tl in
  printf "%d\n" res

let solve_2 input = 
  let width = 101 in
  let height = 103 in
  for i = 0 to 10000 do 
    let arr = Array.init height ~f:(fun _ -> Array.init width ~f:(const ' ')) in
    List.iter input ~f:(fun r -> 
      let p = get_robot_pos i width height r in
      arr.(p.y).(p.x) <- '#'   
    );
    printf "==============================================\n\n%d\n%!" i;
    Array.iter arr ~f:(fun row ->
      Array.iter row ~f:(fun c -> printf "%c" c);
      printf "\n"
    );
    let _ = In_channel.input_line In_channel.stdin in
    ()
  done

let parse_input lines =
  let combined = String.concat ~sep:" " lines in
  let open Re in
  let exp =
    seq
      [
        str "p=";
        group (rep1 digit);
        str ",";
        group (rep1 digit);
        str " v=";
        group (rep1 (alt [ digit; str "-" ]));
        str ",";
        group (rep1 (alt [ digit; str "-" ]));
      ]
  in
  let matches = all (compile exp) combined in
  let get_int g i = Int.of_string (Group.get g i) in
  List.map matches ~f:(fun g ->
      {
        p = { x = get_int g 1; y = get_int g 2 };
        v = { x = get_int g 3; y = get_int g 4 };
      })

let () = Aoc.run_day solve_1 solve_2 parse_input
