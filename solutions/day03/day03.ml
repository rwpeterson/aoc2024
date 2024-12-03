let search_next_mul s i =
  let j = match Str.search_forward (Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}) s i with
  | k -> Some k
  | exception Not_found -> None in
  match j with
  | None -> (None, i)
  | Some k -> begin
    let a = int_of_string @@ Str.matched_group 1 s in
    let b = int_of_string @@ Str.matched_group 2 s in
    (Some (a * b), k)
  end

type program_state =
  | Do
  | Dont

let string_of_program_state = function
  | Do -> "Do"
  | Dont -> "Don't"

let search_next_conditional s i =
  match Str.search_forward (Str.regexp {|do()\|don't()|}) s i with
  | exception Not_found -> (None, i)
  | k -> match Str.matched_string s with
    | "do()" -> (Some Do, k)
    | "don't()" -> (Some Dont, k)
    | _ -> failwith "bad regexp match"

let part1 input =
  let s = Utils.read_file input in
  let len = String.length s in
  let sum = ref 0 in
  let i = ref 0 in
  while !i < len do
    match search_next_mul s !i with
    | (None, _) -> i := len
    | (Some p, j) -> i := j + 1; sum := !sum + p;
  done;
  !sum

let part2 input =
  let s = Utils.read_file input in
  let len = String.length s in
  let sum = ref 0 in
  let i = ref 0 in
  let state = ref Do in
  while !i < len do
    match !state with
    | Dont -> begin match search_next_conditional s !i with
      | None, _ -> i := len
      | Some Dont, j -> i := j + 1
      | Some Do, j -> i := j + 1; state := Do
    end
    | Do -> begin
      let (mul_opt, mul_pos) = search_next_mul s !i in
      let (cnd_opt, cnd_pos) = search_next_conditional s !i in
      match (mul_opt, cnd_opt) with
      | None, _ -> i := len
      | Some p, None -> sum := !sum + p; i := mul_pos + 1
      | Some p, Some c -> begin
        if mul_pos < cnd_pos then begin
          sum := !sum + p; i := mul_pos + 1
        end
        else begin
          state := c; i := cnd_pos + 1
        end
      end
    end
  done;
  !sum

let solve filename =
  let p1 = part1 filename in
  let p2 = part2 filename in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2
