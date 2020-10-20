open Longest_words_eddy
open OUnit2

let () = run_test_tt_main ("longest_words sentence" >::: [

  "no words" >:: (fun _ ->
    assert_equal [] (longest_words "");
    assert_equal [] (longest_words " ");
  );

  "a single word" >:: (fun _ ->
    assert_equal ["word"] (longest_words "word");
  );

  "multiple words, no repeats" >:: (fun _ ->
    let words = longest_words "A long word" in
      assert (words = ["long"; "word"] || words = ["word"; "long"]);
    assert_equal ["sentence"] (longest_words "A short sentence");
  );

  "multiple words, with repeats" >:: (fun _ ->
    assert_equal ["words"] (longest_words "Words words words");
  );

])
