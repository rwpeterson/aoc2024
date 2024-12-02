let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_lines filename =
  read_file filename
  |> String.split_on_char '\n'

let split_on_char sep s =
  String.split_on_char sep s
