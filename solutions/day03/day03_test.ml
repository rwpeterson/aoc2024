let test_part1 () =
  (*Alcotest.(check int) "part1" 0 (Day01.part1 [])*)
  ()

let () =
  Alcotest.run "Day03" [
    "part1", [
      Alcotest.test_case "basic_test" `Quick test_part1;
    ];
  ]
