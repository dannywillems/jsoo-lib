open Jsoo_lib

(* Typed arrays *)
module MakeTestTypedArray (Typed_array : TYPED_ARRAY with type elt = int) =
struct
  let run_tests () =
    let array_buffer = ArrayBuffer.make 64 '\000' in
    let typed_array =
      Typed_array.create ~offset:(Number.of_int 0) array_buffer
    in
    assert (Typed_array.byte_length typed_array = Number.of_int 64) ;
    let typed_array =
      Typed_array.create ~offset:(Number.of_int 10) array_buffer
    in
    assert (Typed_array.byte_length typed_array = Number.of_int 54) ;
    let typed_array =
      Typed_array.create
        ~offset:(Number.of_int 10)
        ~length:(Number.of_int 10)
        array_buffer
    in
    assert (
      Typed_array.byte_length typed_array
      = Number.of_int (Number.to_int Typed_array.bytes_per_element * 10) ) ;
    let typed_array = Typed_array.create array_buffer in
    let typed_array = Typed_array.set typed_array 0 72 in
    (* Get first element, using get_exn and get_opt *)
    assert (Typed_array.get_exn typed_array 0 = 72) ;
    assert (Option.get (Typed_array.get_opt typed_array 0) = 72) ;
    (* Out of bounds *)
    assert (Option.is_none (Typed_array.get_opt typed_array 345435345))
end

module TestUint8Array = MakeTestTypedArray (Uint8TypedArray)
module TestUint16Array = MakeTestTypedArray (Uint16TypedArray)

let () =
  assert (Uint8TypedArray.bytes_per_element = Number.of_int 1) ;
  assert (Uint16TypedArray.bytes_per_element = Number.of_int 2) ;
  TestUint8Array.run_tests () ;
  TestUint16Array.run_tests ()

let () =
  (* Test to_bytes of Uint8TypedArray. Will write a buffer of 11 ASCII
     characters for hello world, and read these bytes as a string
  *)
  let array_buffer = ArrayBuffer.make 11 '\000' in
  let typed_array = Uint8TypedArray.create array_buffer in
  ignore @@ Uint8TypedArray.set typed_array 0 72 ;
  (* H *)
  ignore @@ Uint8TypedArray.set typed_array 1 101 ;
  (* e *)
  ignore @@ Uint8TypedArray.set typed_array 2 108 ;
  (* l *)
  ignore @@ Uint8TypedArray.set typed_array 3 108 ;
  (* l *)
  ignore @@ Uint8TypedArray.set typed_array 4 111 ;
  (* o *)
  ignore @@ Uint8TypedArray.set typed_array 5 32 ;
  (* [space] *)
  ignore @@ Uint8TypedArray.set typed_array 6 119 ;
  (* w *)
  ignore @@ Uint8TypedArray.set typed_array 7 111 ;
  (* o *)
  ignore @@ Uint8TypedArray.set typed_array 8 114 ;
  (* r *)
  ignore @@ Uint8TypedArray.set typed_array 9 108 ;
  (* l *)
  ignore @@ Uint8TypedArray.set typed_array 10 100 ;
  (* d *)
  assert (Bytes.to_string (Uint8TypedArray.to_bytes typed_array) = "Hello world")

(* array buffer *)
let () = assert (ArrayBuffer.(is_array_buffer (to_any_js (make 10 '\000'))))
