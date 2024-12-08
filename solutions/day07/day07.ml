let spaced_ints xs =
  xs
  |> Str.split (Str.regexp " ")
  |> List.map int_of_string

let parse_input s =
  s
  |> List.filter (fun s -> s <> "")
  |> List.map @@ String.split_on_char ':'
  |> List.map (fun x -> List.hd x, List.hd @@ List.tl x)
  |> List.map (fun (a, b) ->(int_of_string a, spaced_ints b))

let gen_ops n =
  let n_ops = n - 1 in
  let rec gen_combs remaining acc =
    if remaining = 0 then
      [acc]
    else
      List.concat [
        gen_combs (remaining - 1) (Int.add :: acc);
        gen_combs (remaining - 1) (Int.mul :: acc);
      ]
    in
    match n with
    | 0 | 1 -> []
    | 2 -> [[Int.add]; [Int.mul]]
    | _ -> gen_combs n_ops []

let concat a b =
  let a' = string_of_int a in
  let b' = string_of_int b in
  int_of_string @@ a' ^ b'

let gen_ops3 n =
  let n_ops = n - 1 in
  let rec gen_combs remaining acc =
    if remaining = 0 then
      [acc]
    else
      List.concat [
        gen_combs (remaining - 1) (Int.add :: acc);
        gen_combs (remaining - 1) (Int.mul :: acc);
        gen_combs (remaining - 1) (concat :: acc);
      ]
    in
    match n with
    | 0 | 1 -> []
    | 2 -> [[Int.add]; [Int.mul]]
    | _ -> gen_combs n_ops []
let valid_ops ops eq =
  let res, operands = eq in
  if List.length ops + 1 <> List.length operands then
    failwith "wrong numbers of stuff";
  let res' = List.fold_left
    (fun acc (op, v) -> op acc v)
    (List.hd operands)
    (List.combine ops (List.tl operands)) in
  res = res'

let valid eq =
  let res, operands = eq in
  let len = List.length operands in
  let all_ops = gen_ops len in
  match List.exists (fun ops -> valid_ops ops eq) all_ops with
  | true -> Some res
  | false -> None

let valid3 eq =
  let res, operands = eq in
  let len = List.length operands in
  let all_ops = gen_ops3 len in
  match List.exists (fun ops -> valid_ops ops eq) all_ops with
  | true -> Some res
  | false -> None

let part1 input =
  let s = Utils.read_lines input in
  let equations = parse_input s in
  List.fold_left (+) 0 (List.filter_map valid equations)

let part2 input =
  let s = Utils.read_lines input in
  let equations = parse_input s in
  List.fold_left (+) 0 (List.filter_map valid3 equations)

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
