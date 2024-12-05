let parse_rule s =
  s
  |> String.split_on_char '|'
  |> List.map int_of_string
  |> fun x -> List.hd x, List.hd @@ List.tl x

let parse_input s =
  let l = Str.split (Str.regexp "\n\n") s in
  let rules_s = List.hd l in
  let updates_s = List.hd @@ List.tl l in
  let rules = Hashtbl.create 1024 in
  let () =
    rules_s
    |> String.split_on_char '\n'
    |> List.map parse_rule
    |> List.iter @@ fun (k, v) -> Hashtbl.add rules k v
  in
  let updates =
    updates_s
    |> String.split_on_char '\n'
    |> List.filter (fun x -> x <> "")
    |> List.map (String.split_on_char ',')
    |> List.map (List.map int_of_string)
  in
  rules, updates

let rec correct_order rules u =
  match u with
  | [] -> true
  | _::[] -> true
  | hd::tl -> match List.for_all (fun x -> List.mem x (Hashtbl.find_all rules hd)) tl with
    | true -> correct_order rules tl
    | false -> false

let replace_nth lst n y =
  let rec aux i = function
  | [] -> []
  | x :: xs ->
    if i = n then y :: xs
    else x :: aux (i + 1) xs
  in
  aux 0 lst

let rec fix_order rules u =
  match u with
  | [] -> u
  | _::[] -> u
  | hd::tl -> begin
    match List.find_index (fun z -> not (List.mem z (Hashtbl.find_all rules hd))) tl with
    | Some i -> fix_order rules ((List.nth tl i)::(replace_nth tl i hd))
    | None -> hd::(fix_order rules tl)
  end

let get_middle_val xs =
  let len = List.length xs in
  List.nth xs (len / 2)

let part1 input =
  let s = Utils.read_file input in
  let rules, updates = parse_input s in
  updates
  |> List.filter @@ correct_order rules
  |> List.map get_middle_val
  |> List.fold_left (fun acc x -> acc + x) 0

let part2 input =
  let s = Utils.read_file input in
  let rules, updates = parse_input s in
  updates
  |> List.filter (fun xs -> not @@ correct_order rules xs)
  |> List.map @@ fix_order rules
  |> List.map get_middle_val
  |> List.fold_left (fun acc x -> acc + x) 0

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
