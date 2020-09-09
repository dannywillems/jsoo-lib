module Memory : sig
  module Buffer = Jsoo_lib.Uint8TypedArray

  val get_buffer : string -> Buffer.t
end

module U64 = Jsoo_lib.BigInt
module U32 = Jsoo_lib.Number
module U16 = Jsoo_lib.Number
module U8 = Jsoo_lib.Number
module I64 = Jsoo_lib.BigInt
module I32 = Jsoo_lib.Number
module Usize = Jsoo_lib.BigInt
