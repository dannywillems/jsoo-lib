module Memory : sig
  include Jsoo_lib.JS_OBJECT

  val get_memory : Jsoo_lib.ESModule.t -> t

  module Buffer = Jsoo_lib.Uint8TypedArray

  (** [get_buffer m] returns the memory buffer. [m] is the ES module
      wasm-bindgen produced *)
  val get_buffer : Jsoo_lib.ESModule.t -> Buffer.t

  val copy_in_buffer : Buffer.t -> Bytes.t -> int -> int -> int -> unit
end

module U64 = Jsoo_lib.BigInt
module U32 = Jsoo_lib.Number
module U16 = Jsoo_lib.Number
module U8 = Jsoo_lib.Number
module I64 = Jsoo_lib.BigInt
module I32 = Jsoo_lib.Number
module Usize = Jsoo_lib.BigInt
