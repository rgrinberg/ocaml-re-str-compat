open OUnit2

module Make (Str : module type of Str) = struct
  let partial_match_test () =
    let re = Str.regexp "[0-9]+K" in
    Str.string_partial_match re "12" 0

  let simple_match () =
    let re = Str.regexp "[0-9]+" in
    Str.string_match re "123" 0
end

module Test_str = Make(Str)
module Test_re = Make(Re_str)

let force f =
  try `Ok (f ())
  with exn -> `Exn exn

let make_test name r1 r2=
  let test _ =
    assert_equal (force r1) (force r2)
  in name >:: test

let test_fixtures =
  "test compat with str" >:::
  [
    (make_test "Test simple match"
       Test_str.simple_match Test_re.simple_match);
    (make_test "Test partial matching"
       Test_str.partial_match_test Test_re.partial_match_test);
  ]

let _ = run_test_tt_main test_fixtures
