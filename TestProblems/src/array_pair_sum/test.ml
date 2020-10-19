open Array_pair_sum_eddy
open OUnit2

let () = run_test_tt_main ("array_pair_sum xs sum" >::: [

  "empty" >:: (fun _ ->
    assert_equal [] (array_pair_sum [] 10);
  );

  "non-empty; without duplicates" >:: (fun _ ->
    assert_equal [(3, 7); (4, 6)] (array_pair_sum [3; 4; 5; 6; 7] 10);
    );


  "non-empty; with duplicates" >:: (fun _ ->
    assert_equal [(1, 3)] (array_pair_sum [1; 1; 2; 3; 4] 4);
    assert_equal [(4, 4); (3, 5)] (array_pair_sum [3; 4; 5; 4; 4] 8);
    assert_equal [(5,5);(3,7);(4,6)] (array_pair_sum []  [3; 4; 5; 6; 4;4;4;7;1;5;5;6;6] 10);
    );

])
