# jsoo_lib

Binding to different JavaScript libraries/routines using js_of_ocaml

```ocaml
let onload _ =
  let btn = Jsoo_lib.get_button_by_id "hello_world" in
  Jsoo_lib.console_log (Js.to_string btn##.innerHTML);
  Jsoo_lib.alert "Hello, World in alert with jsoo_lib"

let () =
  Jsoo_lib.onload onload
```

## How to install?

You need to pin the repository:
```
opam pin add jsoo-lib https://github.com/dannywillems/jsoo-lib.git
```

## Missing stubs

Don't forget to add the stubs related to ocaml-integers. See [tests](test/dune)
for compilation instructions.
