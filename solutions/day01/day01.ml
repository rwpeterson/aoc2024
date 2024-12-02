let part1 input =
  let ls = Utils.read_lines input in
  let aa, bb = ls
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun x -> List.filter (fun x -> x <> "") x)
  |> List.map (fun xs -> List.map (fun x -> int_of_string x) xs)
  |> List.map (function
    | x :: y :: _ -> (x, y)
    | _ -> failwith "list too short"
  )
  |> List.split in
  let cc = List.sort compare aa in
  let dd = List.sort compare bb in
  let sum = List.fold_left2 (fun acc a b -> acc + abs(a - b)) 0 cc dd in
  sum


let part2 input =
  let ls = Utils.read_lines input in
  let aa, bb = ls
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun x -> List.filter (fun x -> x <> "") x)
  |> List.map (fun xs -> List.map (fun x -> int_of_string x) xs)
  |> List.map (function
    | x :: y :: _ -> (x, y)
    | _ -> failwith "list too short"
  )
  |> List.split in
  let score = List.fold_left
    (fun acc x -> acc +  x * (bb
      |> List.filter (fun y -> y == x)
      |> List.fold_left (fun acc2 _ -> acc2 + 1) 0
    ))
    0 aa in
  score

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
