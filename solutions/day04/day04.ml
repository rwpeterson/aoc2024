let make_grid s =
  s
  |> String.split_on_char '\n'
  |> List.map String.to_seq
  |> List.map Array.of_seq
  |> Array.of_list

let get g x y =
  try g.(y).(x)
  with _ -> '0'

let next_char = function
  | 'X' -> 'M'
  | 'M' -> 'A'
  | 'A' -> 'S'
  | 'S' -> '1'
  | _ -> '0'

let rec follow_word g i j d c =
  let next = next_char c in
  let ch = get g (i + fst d) (j + snd d) in
  match next, ch with
  | 'M', 'M' -> follow_word g (i + fst d) (j + snd d) d 'M'
  | 'A', 'A' -> follow_word g (i + fst d) (j + snd d) d 'A'
  | 'S', 'S' -> 1
  |  _ , '0' -> 0
  |  _ ,  _  -> 0

let check_mas_stamp g i j ds =
  let m1 = get g (i + fst ds.(0)) (j + snd ds.(0)) in
  let m2 = get g (i + fst ds.(1)) (j + snd ds.(1)) in
  let s1 = get g (i + fst ds.(2)) (j + snd ds.(2)) in
  let s2 = get g (i + fst ds.(3)) (j + snd ds.(3)) in
  match m1 = 'M' && m2 = 'M' && s1 = 'S' && s2 = 'S' with
  | true -> 1
  | false -> 0

let count_dirs g i j =
  let dirs = [|
     0,  1;
     0, -1;
     1,  0;
    -1,  0;
     1,  1;
     1, -1;
    -1,  1;
    -1, -1;
  |] in
  Array.fold_left (fun acc d -> acc + follow_word g i j d 'X') 0 dirs

let count_mas_x g i j =
  let dirs = [|
    [|(-1, 1); (-1, -1); ( 1, 1); ( 1, -1)|]; (* left Ms *)
    [|( 1, 1); ( 1, -1); (-1, 1); (-1, -1)|]; (* right Ms *)
    [|(-1, 1); ( 1,  1); (-1,-1); ( 1, -1)|]; (* up Ms *)
    [|(-1,-1); ( 1, -1); (-1, 1); ( 1,  1)|]; (* down Ms *)
  |] in
  Array.fold_left (fun acc ds -> acc + check_mas_stamp g i j ds) 0 dirs

let check_cell g i j =
  match get g i j with
  | 'X' -> count_dirs g i j
  | _ -> 0

let check_mas g i j =
  match get g i j with
  | 'A' -> count_mas_x g i j
  | _ -> 0

let count_xmases g = 
  let c = ref 0 in
  Array.iteri (fun j xs -> Array.iteri (fun i _ -> c := !c + check_cell g i j) xs) g;
  !c

let count_mases g =
  let c = ref 0 in
  Array.iteri (fun j xs -> Array.iteri (fun i _ -> c := !c + check_mas g i j) xs) g;
  !c

let part1 input =
  let s = Utils.read_file input in
  let g = make_grid s in
  count_xmases g

let part2 input =
  let s = Utils.read_file input in
  let g = make_grid s in
  count_mases g

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
