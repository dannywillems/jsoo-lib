open Js_of_ocaml.Js
open Jsoo_lib

let () =
  let fs = ESModule.of_js (Unsafe.js_expr {| require("os") |}) in
  let tmp_dir : js_string t = Unsafe.coerce (ESModule.call fs "tmpdir" [||]) in
  print_endline (to_string tmp_dir)
