let () =
  assert (
    Unsigned.UInt32.equal
      (Unsigned.UInt32.of_int 42)
      Jsoo_lib.Number.(to_uint32 (of_int 42)) ) ;
  assert (
    Unsigned.UInt64.equal
      (Unsigned.UInt64.of_int 42)
      Jsoo_lib.Number.(to_uint64 (of_int 42)) ) ;
  assert (Int.equal 42 Jsoo_lib.Number.(to_int (of_int 42))) ;
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_int 10)))) ;
  assert (Jsoo_lib.Number.(is_number (to_any_js (of_string "234234")))) ;
  assert (
    Jsoo_lib.Number.(
      is_number (to_any_js (of_uint32 (Unsigned.UInt32.of_int 10)))) ) ;
  assert (
    Jsoo_lib.Number.(
      is_number (to_any_js (of_uint64 (Unsigned.UInt64.of_int 10)))) )
