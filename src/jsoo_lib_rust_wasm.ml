module Memory = struct
  let get_memory_object m =
    let open Js_of_ocaml.Js in
    Unsafe.get (Unsafe.get m "__wasm") "memory"

  module Buffer = Jsoo_lib.Uint8TypedArray

  let get_buffer m =
    let open Js_of_ocaml.Js in
    Buffer.create
      (Jsoo_lib.ArrayBuffer.of_js
         (Unsafe.get
            (get_memory_object (Jsoo_lib.ESModule.to_any_js m))
            "buffer"))
end

module U64 = Jsoo_lib.BigInt
module U32 = Jsoo_lib.Number
module U16 = Jsoo_lib.Number
module U8 = Jsoo_lib.Number
module I64 = Jsoo_lib.BigInt
module I32 = Jsoo_lib.Number
module Usize = Jsoo_lib.BigInt
