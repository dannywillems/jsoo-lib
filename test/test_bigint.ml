let () =
  assert (Jsoo_lib.BigInt.(is_bigint (to_any_js (of_int 10)))) ;
  assert (Jsoo_lib.BigInt.(is_bigint (to_any_js (of_string "234234"))))
