module Memory : sig
  module Buffer = Jsoo_lib.Uint8TypedArray

  (** [get_buffer m] returns the memory buffer. [m] is the ES module
      wasm-bindgen produced *)
  val get_buffer : Jsoo_lib.ESModule.t -> Buffer.t
end

module U64 = Jsoo_lib.BigInt
module U32 = Jsoo_lib.Number
module U16 = Jsoo_lib.Number
module U8 = Jsoo_lib.Number
module I64 = Jsoo_lib.BigInt
module I32 = Jsoo_lib.Number
module Usize = Jsoo_lib.BigInt
