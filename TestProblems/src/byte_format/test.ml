open Byte_format_eddy
open OUnit2

let () = run_test_tt_main ("byte_format n precision" >::: [

  "`n` < 0" >:: (fun _ ->
        assert_raises (Failure "n < 0") (fun () -> byte_format (-1));
        assert_raises (Failure "n < 0") (fun () -> byte_format (-2));
  );

  "`precision` < 0" >:: (fun _ ->
      assert_raises (Failure "precision < 0") (fun () -> byte_format ~precision: (-3) 12331);
      assert_raises (Failure "precision < 0") (fun () -> byte_format ~precision:(-1) (-5) );
    );

  "B" >:: (fun _ ->
    let n = 42 in
    assert_equal "42 B" (byte_format ~precision:0 n );
    assert_equal "42.0 B" (byte_format ~precision:1 n);
    assert_equal "42.000 B" (byte_format ~precision:3 n);
  );

  "KB" >:: (fun _ ->
    let n = 8101 in
    assert_equal "8 KB" (byte_format ~precision:0 n);
    assert_equal "7.9 KB" (byte_format ~precision:1 n);
    assert_equal "7.911 KB" (byte_format ~precision:3 n)
  );

  "MB" >:: (fun _ ->
    let n = 156833213 in
    assert_equal "150 MB" (byte_format ~precision:0 n);
    assert_equal "149.6 MB" (byte_format ~precision:1 n);
    assert_equal "149.568 MB" (byte_format ~precision:3 n)
  );

  "GB" >:: (fun _ ->
    let n = 9876543210 in
    assert_equal "9 GB" (byte_format ~precision:0 n);
    assert_equal "9.2 GB" (byte_format ~precision:1 n);
    assert_equal "9.198 GB" (byte_format ~precision:3 n)
  );

  "TB" >:: (fun _ ->
    let n = 123456789123456789 in
    assert_equal "112283 TB" (byte_format ~precision:0 n);
    assert_equal "112283.3 TB" (byte_format ~precision:1 n);
    assert_equal "112283.296 TB" (byte_format ~precision:3 n)
  );

])
