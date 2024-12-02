type trend =
  | First
  | Second
  | Up
  | Down
  | Unsafe

type trend_count = { up : int; down : int; unsafe : int}

let still_up p c = match c - p with
  | 1 | 2 | 3 -> Up
  | _ -> Unsafe

let still_down p c = match c - p with
  | -1 | -2 | -3 -> Down
  | _ -> Unsafe

let set_trend p c = match c - p with
  | 1 | 2 | 3 -> Up
  | -1 | -2 | -3 -> Down
  | _ -> Unsafe

let keep_trend t p c =
  match t with
  | First -> Second, c
  | Second -> set_trend p c, c
  | Up -> still_up p c, c
  | Down -> still_down p c, c
  | Unsafe -> Unsafe, c

let track_trend t p c tc =
  match t with
  | First -> Second, c, tc
  | Second | Up | Down | Unsafe -> match set_trend p c with
    | Up -> Up, c, { tc with up = tc.up + 1 }
    | Down -> Down, c, { tc with down = tc.down + 1 }
    | Unsafe -> Unsafe, c, { tc with unsafe = tc.unsafe + 1 }
    | _ -> failwith "unreachable"

let find_trend xs =
  let (t, _) = List.fold_left (fun (acc_t, acc_x) x -> keep_trend acc_t acc_x x) (First, 0) xs in
  t

let print_trend xs = match find_trend xs with
  | First -> Printf.printf "First\n"
  | Second -> Printf.printf "Second\n"
  | Up -> Printf.printf "Up\n"
  | Down -> Printf.printf "Down\n"
  | Unsafe -> Printf.printf "Unsafe\n"

let safe_trend xs =
  match find_trend xs with
  | First | Second -> failwith "first/second not possible after processing"
  | Up -> true
  | Down -> true
  | Unsafe -> false

let full_and_dropouts xs =
  let drop_elem xs j =
    xs
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (i, _) -> i <> j)
    |> List.map snd
  in
  xs :: (List.mapi (fun i _ -> drop_elem xs i) xs)

let part1 input =
  Utils.read_lines input
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun xs -> List.map int_of_string xs)
  (* |> List.filter (fun xs -> print_trend xs; safe_trend xs) *)
  |> List.filter safe_trend
  |> List.fold_left (fun acc _ -> acc + 1) 0

let part2 input =
  Utils.read_lines input
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun xs -> List.map int_of_string xs)
  |> List.map (fun xs -> full_and_dropouts xs)
  |> List.map (fun xss -> List.exists safe_trend xss)
  |> List.fold_left (fun acc b -> if b then acc + 1 else acc) 0

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
