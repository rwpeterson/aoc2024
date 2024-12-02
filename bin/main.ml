let get_input_file day example =
  Printf.sprintf "solutions/day%02d/%sinput.txt" day
    (if example then "example_" else "")

let time f =
  let t0 = Sys.time () in
  let result = f () in
  let t1 = Sys.time () in
  (result, t1 -. t0)

let solve_day day example =
  match day with
  | 1 -> Day01.solve (get_input_file day example)
  | 2 -> Day02.solve (get_input_file day example)
  | n -> Printf.printf "Day %d not implemented yet\n" n

let () =
  let day = ref 1 in
  let example = ref false in
  let speclist = [
    ("-d", Arg.Set_int day, "Day to run (default: 1)");
    ("-e", Arg.Set example, "Use example input");
  ] in
  let usage_msg = "Usage: dune exec aoc [-d day] [-e]" in
  Arg.parse speclist (fun _ -> ()) usage_msg;

  Printf.printf "Running Day %d with %s input\n"
    !day (if !example then "example" else "actual");

  let ((), elapsed) = time (fun () -> solve_day !day !example) in
  Printf.printf "Time: %.3f seconds\n" elapsed
