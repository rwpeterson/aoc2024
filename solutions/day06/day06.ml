open Either

type direction =
  N | S | E | W

let next_direction = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N

type position = {
  visits: int;
  guard: direction option;
}

type map = {
  m: (position, unit) t array array;
  mutable i: int;
  mutable j: int;
}

let parse_start = function
  | '#' -> Right ()
  | '.' -> Left { visits = 0; guard = None }
  | '^' -> Left { visits = 0; guard = Some N }
  | 'v' -> Left { visits = 0; guard = Some S }
  | '>' -> Left { visits = 0; guard = Some E }
  | '<' -> Left { visits = 0; guard = Some W }
  | _ -> failwith "unknown starting state"

let pos_has_guard = function
  | Right () | Left { visits = _; guard = None } -> false
  | Left { visits = _; guard = Some _ } -> true

let row_has_guard r =
  Array.exists pos_has_guard r

let make_map s =
  let m = s
  |> String.split_on_char '\n'
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> List.map @@ List.map parse_start
  |> List.map Array.of_list
  |> Array.of_list in
  let j = match Array.find_index row_has_guard m with
  | Some jj -> jj
  | None -> failwith "has to have a starting row" in
  let i = match Array.find_index pos_has_guard m.(j) with
  | Some ii -> ii
  | None -> failwith "has to have a starting column" in
  { m = m; i = i; j = j }

let turn_right m =
  match m.m.(m.j).(m.i) with
  | Right () -> failwith "can't be inside an obstruction"
  | Left { visits = _; guard = None } -> failwith "can't not be a guard where guard is"
  | Left { visits = v; guard = Some d } -> begin
    m.m.(m.j).(m.i) <- Left { visits = v; guard = Some (next_direction d) }
    end

let go_there m i j k l =
  match m.m.(m.j).(m.i) with
  | Right () -> failwith "can't be inside an obstruction"
  | Left { visits = _; guard = None } -> failwith "can't not be a guard where guard is"
  | Left { visits = v; guard = Some d } -> begin
    m.m.(j).(i) <- Left { visits = v + 1; guard = None };
    m.m.(l).(k) <- begin
      let v0 = match m.m.(l).(k) with
        | Right () -> failwith "same same"
        | Left { visits = vv; guard = _ } -> vv in
      Left { visits = v0; guard = Some d }
    end;
    m.i <- k;
    m.j <- l;
    end

let rec run_route m =
  match m.m.(m.j).(m.i) with
  | Right () -> failwith "can't be inside an obstruction"
  | Left { visits = _; guard = None } -> failwith "can't not be a guard where guard is"
  | Left { visits = _; guard = Some d } -> begin match d with
    | N -> begin match m.m.(m.j - 1).(m.i) with
      | Right () -> turn_right m; run_route m
      | Left { visits = _; guard = _ } -> go_there m m.i m.j m.i (m.j - 1); run_route m
      | exception Invalid_argument _ -> m
      end
    | S -> begin match m.m.(m.j + 1).(m.i) with
      | Right () -> turn_right m; run_route m
      | Left { visits = _; guard = _ } -> go_there m m.i m.j m.i (m.j + 1); run_route m
      | exception Invalid_argument _ -> m
      end
    | E -> begin match m.m.(m.j).(m.i + 1) with
      | Right () -> turn_right m; run_route m
      | Left { visits = _; guard = _ } -> go_there m m.i m.j (m.i + 1) m.j; run_route m
      | exception Invalid_argument _ -> m
      end
    | W -> begin match m.m.(m.j).(m.i - 1) with
      | Right () -> turn_right m; run_route m
      | Left { visits = _; guard = _ } -> go_there m m.i m.j (m.i - 1) m.j; run_route m
      | exception Invalid_argument _ -> m
      end
  end

let count_spot p tot =
  match p with
  | Right () -> ()
  | Left { visits = v; guard = _ } -> if v > 0 then tot := !tot + 1

let count_row r tot =
  Array.iter (fun p -> count_spot p tot) r

let count_pos m =
  let tot = ref 1 in (* we don't count the final position before stepping off the board *)
  Array.iter (fun r -> count_row r tot) m.m;
  !tot

let part1 input =
  let s = Utils.read_file input in
  let m = make_map s in
  let m = run_route m in
  count_pos m

let part2 input =
  let _ = input in
  0

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
