open Core

let solve_1 input =
  let points = Array.init 256 ~f:(fun _ -> Dynarray.create ()) in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let result_arr =
    Array.init height ~f:(fun _ -> Array.init width ~f:(const ' '))
  in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if not @@ Char.equal input.(i).(j) '.' then
        Dynarray.add_last points.(Char.to_int input.(i).(j)) (j, i)
    done
  done;
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  Array.iter points ~f:(fun ar ->
      let len = Dynarray.length ar in
      for i = 0 to len - 1 do
        for j = i + 1 to len - 1 do
          let x1, y1 = Dynarray.get ar i in
          let x2, y2 = Dynarray.get ar j in
          let dx, dy = (x1 - x2, y1 - y2) in
          let nx1, ny1 = (x1 + dx, y1 + dy) in
          let nx2, ny2 = (x2 - dx, y2 - dy) in
          if is_safe nx1 ny1 then result_arr.(ny1).(nx1) <- '#';
          if is_safe nx2 ny2 then result_arr.(ny2).(nx2) <- '#';
        done
      done);
  let res =
    Array.fold result_arr ~init:0 ~f:(fun acc row ->
        acc
        + Array.fold row ~init:0 ~f:(fun acc x ->
              if Char.equal x '#' then acc + 1 else acc))
  in
  printf "%d\n" res

let solve_2 input =
  let points = Array.init 256 ~f:(fun _ -> Dynarray.create ()) in
  let height = Array.length input in
  let width = Array.length input.(0) in
  let result_arr =
    Array.init height ~f:(fun _ -> Array.init width ~f:(const ' '))
  in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if not @@ Char.equal input.(i).(j) '.' then
        Dynarray.add_last points.(Char.to_int input.(i).(j)) (j, i)
    done
  done;
  let is_safe x y = x >= 0 && x < width && y >= 0 && y < height in
  let rec gcd x y =
    if y > x then gcd y x 
    else 
      if y = 0 then x
      else gcd y (x%y) in
  Array.iter points ~f:(fun ar ->
      let len = Dynarray.length ar in
      for i = 0 to len - 1 do
        for j = i + 1 to len - 1 do
          let x1, y1 = Dynarray.get ar i in
          let x2, y2 = Dynarray.get ar j in
          let dx_a, dy_a = (x1 - x2, y1 - y2) in
          let g = gcd (abs dx_a) (abs dy_a) in
          let dx = dx_a / g in
          let dy = dy_a / g in
          let x = ref x1 in
          let y = ref y1 in
          while is_safe (!x + dx) (!y + dy) do
            x := !x + dx;
            y := !y + dy
          done;
          while is_safe !x !y do
            result_arr.(!y).(!x) <- '#';
            x := !x - dx;
            y := !y - dy
          done
        done
      done);
  let res =
    Array.fold result_arr ~init:0 ~f:(fun acc row ->
        acc
        + Array.fold row ~init:0 ~f:(fun acc x ->
              if Char.equal x '#' then acc + 1 else acc))
  in
  printf "%d\n" res
let parse_input lines = List.to_array lines |> Array.map ~f:String.to_array
let () = Aoc.run_day solve_1 solve_2 parse_input
