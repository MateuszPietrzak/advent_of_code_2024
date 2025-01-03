open Core

type segment_type = Empty | Full of int

type segment = {
  seg_type : segment_type;
  mutable seg_beg : int;
  mutable seg_end : int;
}

let length s = s.seg_end - s.seg_beg
let get_score x b e = x * (max 0 ((e - 1) * e) - max 0 ((b - 1) * b)) / 2

let solve_1 segments =
  let seg_arr = List.rev segments |> Array.of_list in
  let res = ref 0 in
  let len = Array.length seg_arr in
  let beg_it = ref 0 in
  let end_it = ref (len - 1) in
  let beg_place = ref seg_arr.(!beg_it).seg_beg in
  let end_place = ref seg_arr.(!end_it).seg_beg in
  let cur_segment = ref seg_arr.(!beg_it) in
  while !beg_place < !end_place do
    match !cur_segment.seg_type with
    | Full x ->
        res := !res + get_score x !cur_segment.seg_beg !cur_segment.seg_end;
        Int.incr beg_it;
        beg_place := seg_arr.(!beg_it).seg_beg;
        cur_segment := seg_arr.(!beg_it)
    | Empty ->
        let end_seg = seg_arr.(!end_it) in
        let end_x = match end_seg.seg_type with Full x -> x | Empty -> 0 in
        if length end_seg = length !cur_segment then (
          res :=
            !res + get_score end_x !cur_segment.seg_beg !cur_segment.seg_end;
          end_it := !end_it - 2;
          end_place := seg_arr.(!end_it).seg_beg;
          Int.incr beg_it;
          beg_place := seg_arr.(!beg_it).seg_beg;
          cur_segment := seg_arr.(!beg_it))
        else if length end_seg > length !cur_segment then (
          res :=
            !res + get_score end_x !cur_segment.seg_beg !cur_segment.seg_end;
          seg_arr.(!end_it).seg_end <- end_seg.seg_end - length !cur_segment;
          Int.incr beg_it;
          beg_place := seg_arr.(!beg_it).seg_beg;
          cur_segment := seg_arr.(!beg_it))
        else (
          res :=
            !res
            + get_score end_x !cur_segment.seg_beg
                (!cur_segment.seg_beg + length end_seg);
          end_it := !end_it - 2;
          end_place := seg_arr.(!end_it).seg_beg;
          !cur_segment.seg_beg <- !cur_segment.seg_beg + length end_seg;
          beg_place := !beg_place + length end_seg)
  done;
  let end_seg = seg_arr.(!end_it) in
  let end_x = match end_seg.seg_type with Full x -> x | Empty -> 0 in
  res := !res + get_score end_x end_seg.seg_beg end_seg.seg_end;
  printf "%d\n" !res

let set_value tree ind v =
  let rec aux vert =
    if vert > 0 then (
      tree.(vert) <- max tree.(2 * vert) tree.((2 * vert) + 1);
      aux (vert / 2))
  in
  let ts = 32768 in
  tree.(ind + ts) <- v;
  aux ((ind + ts) / 2)

let get_leftmost tree width =
  let ts = 32768 in
  let rec aux v =
    if v >= ts then v - ts
    else if tree.(v * 2) >= width then aux (v * 2)
    else aux ((v * 2) + 1)
  in
  aux 1

let solve_2 segments =
  let seg_arr = List.rev segments |> Array.of_list in
  let binary_tree = Array.init 70000 ~f:(const 0) in
  List.iteri segments ~f:(fun i s ->
      let it = List.length segments - i - 1 in
      let leaf_size = match s.seg_type with Empty -> length s | Full _ -> 0 in
      set_value binary_tree it leaf_size);
  List.iteri segments ~f:(fun i s ->
      match s.seg_type with
      | Full _ ->
          let it = List.length segments - i - 1 in
          let fit = get_leftmost binary_tree (length s) in
          if fit < it then (
            let l = length seg_arr.(it) in
            seg_arr.(it).seg_beg <- seg_arr.(fit).seg_beg;
            seg_arr.(it).seg_end <- seg_arr.(fit).seg_beg + l;
            seg_arr.(fit).seg_beg <- seg_arr.(fit).seg_beg + l;
            set_value binary_tree fit (length seg_arr.(fit)));
          set_value binary_tree it 0
      | Empty -> ());
  let res =
    Array.fold seg_arr ~init:0 ~f:(fun acc s ->
        match s.seg_type with
        | Empty -> acc
        | Full x -> acc + get_score x s.seg_beg s.seg_end)
  in
  printf "%d\n" res

let parse_input lines =
  let _, segs =
    List.hd_exn lines |> String.to_list
    |> List.foldi ~init:(0, []) ~f:(fun i (sum, acc) e ->
           let digit = Char.to_int e - Char.to_int '0' in
           if i % 2 = 0 then
             ( sum + digit,
               { seg_type = Full (i / 2); seg_beg = sum; seg_end = sum + digit }
               :: acc )
           else
             ( sum + digit,
               { seg_type = Empty; seg_beg = sum; seg_end = sum + digit } :: acc
             ))
  in
  segs

let () = Aoc.run_day solve_1 solve_2 parse_input
