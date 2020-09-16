module Memory = struct
  include Jsoo_lib.Js_object_base

  let get_memory m : t =
    let open Js_of_ocaml.Js in
    Unsafe.get (Unsafe.get m "wasm") "memory"

  module Buffer = Jsoo_lib.Uint8TypedArray

  let get_buffer m =
    let open Js_of_ocaml.Js in
    Buffer.create
      (Jsoo_lib.ArrayBuffer.of_js
         (Unsafe.get (get_memory (Jsoo_lib.ESModule.to_any_js m)) "buffer"))

  let copy_in_buffer buffer src src_offset offset_in_buffer len =
    assert (
      Jsoo_lib.Number.to_int (Buffer.byte_length buffer)
      >= offset_in_buffer + len ) ;
    let rec aux i =
      if i = len then ()
      else (
        ignore
        @@ Buffer.set
             buffer
             (offset_in_buffer + i)
             (Bytes.get_uint8 src (src_offset + i)) ;
        aux (i + 1) )
    in
    aux 0
end

module U64 = Jsoo_lib.BigInt
module U32 = Jsoo_lib.Number
module U16 = Jsoo_lib.Number
module U8 = Jsoo_lib.Number
module I64 = Jsoo_lib.BigInt
module I32 = Jsoo_lib.Number
module Usize = Jsoo_lib.BigInt
