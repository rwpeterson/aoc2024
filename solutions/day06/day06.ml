open Either

type direction =
  N | S | E | W

let next_direction = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N

type position = {
  visits: int option;
  guard: direction option;
}

type map = {
  m: (position, unit) t array array;
  mutable i: int;
  mutable j: int;
  mutable max_rpts: (int * int) * int;
}

let parse_start = function
  | '#' -> Right ()
  | '.' -> Left { visits = None; guard = None }
  | '^' -> Left { visits = None; guard = Some N }
  | 'v' -> Left { visits = None; guard = Some S }
  | '>' -> Left { visits = None; guard = Some E }
  | '<' -> Left { visits = None; guard = Some W }
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
  { m = m; i = i; j = j; max_rpts = ((0, 0), 0) }

let deep_copy (a : 'a array array) =
  Array.init (Array.length a) (fun j -> Array.copy a.(j))

let copy_map m =
  { m = deep_copy m.m; i = m.i; j = m.j; max_rpts = m.max_rpts }

let turn_right m =
  match m.m.(m.j).(m.i) with
  | Right () -> failwith "can't be inside an obstruction"
  | Left { visits = _; guard = None } -> failwith "can't not be a guard where guard is"
  | Left { visits = v; guard = Some d } -> begin
    m.m.(m.j).(m.i) <- Left { visits = v; guard = Some (next_direction d) }
    end

exception Cyclic

let go_there m i j k l =
  match m.m.(m.j).(m.i) with
  | Right () -> failwith "can't be inside an obstruction"
  | Left { visits = _; guard = None } -> failwith "can't not be a guard where guard is"
  | Left { visits = v; guard = Some d } -> begin
    m.m.(j).(i) <- Left { visits = Some ((try Option.get v with _ -> 0) + 1); guard = None };
    m.m.(l).(k) <- begin
      let v0 = match m.m.(l).(k) with
        | Right () -> failwith "same same"
        | Left { visits = w; guard = _ } -> w in
      begin match v0 with
      | Some u -> begin
        if u > snd m.max_rpts then
          m.max_rpts <- ((k, l), u);
          if u > 8 then (* arb threshold or better constraint? *)
          raise Cyclic
        end
      | None -> ()
      end;
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
  | Left { visits = None; guard = _ } -> ()
  | Left { visits = Some _; guard = _ } -> tot := !tot + 1

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
  let s = Utils.read_file input in
  let m' = make_map s in
  let x = Array.length m'.m.(0) in
  let y = Array.length m'.m - 1 in
  Printf.printf "x = %d, y = %d\n" x y;
  let num_cyclic = ref 0 in
  for ii = 0 to x - 1 do
    for jj = 0 to y - 1 do
      let m = copy_map m' in
      match m.m.(jj).(ii) with
      | Right _ | Left { visits = _; guard = None } -> begin
        m.m.(jj).(ii) <- Right ();
        match run_route m with
        | _ -> () (* left the board; not cyclic *)
        | exception Cyclic -> num_cyclic := !num_cyclic + 1;
        | exception Invalid_argument a -> Printf.printf "LMAO: %s\n" a;
        end
      | _ -> () (* don't place obstruction on top of guard starting position *)
    done
  done;
  !num_cyclic

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in (* u > 2: 1790 too high, u > 4: 1767  *)
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
