let () =
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_int 10)))) ;
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_string "234234")))) ;
  assert (
    Jsoo_lib.Number.(
      is_number (to_any_js (of_uint32 (Unsigned.UInt32.of_int 10)))) ) ;
  assert (
    Jsoo_lib.Number.(
      is_number (to_any_js (of_uint64 (Unsigned.UInt64.of_int 10)))) )
