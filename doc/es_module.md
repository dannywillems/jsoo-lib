## ES modules

To use the `require` expression, use
```ocaml
let m = Js.Unsafe.js_expr {|require ("module_name") |} in [...]
```

For instance, to require the `uuid` module, use
```ocaml
let m = Js.Unsafe.js_expr {| require ("uuid" |)} in [...]
```

`Jsoo_lib` provides a typed version of ES modules, `Jsoo_lib.ESModule.t`. You
can use `Jsoo_lib.ESModule.of_any_js` to coerce the result of the above
`js_expr` expression.

```ocaml
let m = Jsoo_lib.ESModule.of_any_js (Js.Unsafe.js_expr {| require ("uuid") |}) in
[...]
```

The import expression can be used in the same way. However, the expression
`import` in JavaScript is a promise. You must then use:

```ocaml
let () =
  let open Js_of_ocaml in
  let open Js_of_ocaml.Js in
  let p : (Jsoo_lib.ESModule.t, Unsafe.any) Jsoo_lib.Promise.t = Jsoo_lib.Promise.of_any_js (Unsafe.js_expr {| import ("uuid") |}) in
  [...]
```

See [documentation about promises](./promises.md) for more information on how to use promises.
