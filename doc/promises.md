## JavaScript promises

Promises are available under `Jsoo_lib.Promise`. A promise is represented by a type `Jsoo_lib.Promise.t`.
For instance, to use `import` asynchronously, you can use

```ocaml
let () =
  let open Js_of_ocaml in
  let open Js_of_ocaml.Js in
  let p : (Jsoo_lib.ESModule.t, Unsafe.any) Jsoo_lib.Promise.t = Jsoo_lib.Promise.of_any_js (Unsafe.js_expr {| import ("uuid") |}) in
  let log_uuid_module uuid_mod = Firebug.console##log (Jsoo_lib.ESModule.to_any_js uuid_mod) in
  Jsoo_lib.Promise.then_bind ~on_resolved:log_uuid_module p
```
