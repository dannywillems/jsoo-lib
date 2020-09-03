let () =
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_int 10)))) ;
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_string "234234"))))
