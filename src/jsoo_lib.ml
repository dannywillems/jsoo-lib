open Js_of_ocaml

module type JS_OBJECT = sig
  type t

  val of_js : Js.Unsafe.any -> t

  val to_any_js : t -> Js.Unsafe.any

  val to_string : t -> string
end

module Js_object_base : JS_OBJECT = struct
  type t = Js.Unsafe.any

  let of_js x = x

  let to_any_js x = x

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])
end

let doc = Dom_html.document

(* -------------------------------------------------------------------------- *)
let get_input_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.input (fun _ ->
      assert false)

let get_p_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.p (fun _ ->
      assert false)

let get_div_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.div (fun _ ->
      assert false)

let get_blockquote_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.blockquote (fun _ ->
      assert false)

let get_button_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.button (fun _ ->
      assert false)

let get_link_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.link (fun _ ->
      assert false)

let get_body_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.body (fun _ ->
      assert false)

let get_img_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.img (fun _ ->
      assert false)

let get_textarea_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.textarea (fun _ ->
      assert false)

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
let console_log str = Firebug.console##log (Js.string str)

let alert str = Dom_html.window##alert (Js.string str)

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
let onload (f : Dom_html.event Js.t -> bool Js.t) =
  Dom_html.window##.onload := Dom.handler f

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
module Head = struct
  (* FIXME: external lib for listing media_type *)
  type media_type = Text_css | Text_js | No_media_type

  type rel =
    | Alternate
    | Archives
    | Author
    | Bookmark
    | External
    | First
    | Help
    | Icon
    | Last
    | License
    | Next
    | Nofollow
    | Noreferrer
    | Pingback
    | Prefetch
    | Prev
    | Search
    | Sidebar
    | Stylesheet
    | Tag
    | Up

  type cross_origin = Anonymous | Use_credentials | Default_cross_origin

  (* Empty argument *)

  let media_type_to_str s =
    match s with
    | Text_css -> "text/css"
    | Text_js -> "text/js"
    | No_media_type -> ""

  let rel_to_str s =
    match s with
    | Alternate -> "alternate"
    | Archives -> "archives"
    | Author -> "author"
    | Bookmark -> "bookmark"
    | External -> "external"
    | First -> "first"
    | Help -> "help"
    | Icon -> "icon"
    | Last -> "last"
    | License -> "license"
    | Next -> "next"
    | Nofollow -> "nofollow"
    | Noreferrer -> "noreferrer"
    | Pingback -> "pingback"
    | Prefetch -> "prefetch"
    | Prev -> "prev"
    | Search -> "search"
    | Sidebar -> "sidebar"
    | Stylesheet -> "stylesheet"
    | Tag -> "tag"
    | Up -> "up"

  let cross_origin_to_str s =
    match s with
    | Anonymous -> "anonymous"
    | Use_credentials -> "use-credentials"
    | Default_cross_origin -> ""

  let add_link ?(media_type = No_media_type)
      ?(cross_origin = Default_cross_origin) ?(language_code = "")
      ?(media_query = "") ~rel href =
    let l = Dom_html.createLink doc in
    l##.crossorigin := Js.string (cross_origin_to_str cross_origin) ;
    l##.href := Js.string href ;
    if language_code <> "" then l##.hreflang := Js.string language_code ;
    if media_query <> "" then l##.media := Js.string media_query ;
    l##.rel := Js.string (rel_to_str rel) ;
    l##._type := Js.string (media_type_to_str media_type) ;
    Dom.appendChild doc##.head l

  let add_css_link href = add_link ~media_type:Text_css ~rel:Stylesheet href

  let add_js_script l =
    let script = Dom_html.createScript doc in
    script##.src := Js.string l ;
    script##._type := Js.string "text/javascript" ;
    Dom.appendChild doc##.head script
end

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
module Body = struct
  let add_js_script l =
    let script = Dom_html.createScript doc in
    script##.src := Js.string l ;
    script##._type := Js.string "text/javascript" ;
    Dom.appendChild doc##.body script

  let rec append_child_mult a l =
    match l with
    | [] -> ()
    | head :: tail ->
        Dom.appendChild a head ;
        append_child_mult a tail
end

(* -------------------------------------------------------------------------- *)

module BigInt = struct
  include Js_object_base

  let of_int x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "BigInt")
      [| Js.Unsafe.inject (Js.string (string_of_int x)) |]

  let zero = of_int 0

  let one = of_int 1

  let of_uint32 x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "BigInt")
      [| Js.Unsafe.inject (Js.string (Unsigned.UInt32.to_string x)) |]

  let of_uint64 x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "BigInt")
      [| Js.Unsafe.inject (Js.string (Unsigned.UInt64.to_string x)) |]

  let of_string x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "BigInt")
      [| Js.Unsafe.inject (Js.string x) |]

  let is_bigint x = Js.to_string (Js.typeof x) = "bigint"

  let to_uint64 x =
    Unsigned.UInt64.of_string
      (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))

  let to_uint32 x =
    Unsigned.UInt32.of_string
      (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))

  let to_int x =
    int_of_string (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))
end

module Number = struct
  include Js_object_base

  let of_int x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "Number")
      [| Js.Unsafe.inject (Js.string (string_of_int x)) |]

  let zero = of_int 0

  let one = of_int 1

  let of_string x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "Number")
      [| Js.Unsafe.inject (Js.string x) |]

  let of_uint32 x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "Number")
      [| Js.Unsafe.inject (Js.string (Unsigned.UInt32.to_string x)) |]

  let of_uint64 x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "Number")
      [| Js.Unsafe.inject (Js.string (Unsigned.UInt64.to_string x)) |]

  let is_number x = Js.to_string (Js.typeof x) = "number"

  let to_uint64 x =
    Unsigned.UInt64.of_string
      (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))

  let to_uint32 x =
    Unsigned.UInt32.of_string
      (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))

  let to_int x =
    int_of_string (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))
end

(* Is already available in Js_of_ocaml.Typed_array. Higher interface with non-JS types *)
module ArrayBuffer = struct
  include Js_object_base

  let make size _c =
    (Js.Unsafe.new_obj (Js.Unsafe.variable "ArrayBuffer"))
      [| Number.to_any_js (Number.of_int size) |]

  let is_array_buffer x =
    Js.to_string (Js.typeof x) = "object"
    && Js.to_string (Js.Unsafe.get (Js.Unsafe.get x "constructor") "name")
       = "ArrayBuffer"

  let length x = Number.of_js (Js.Unsafe.get x "byteLength")
end

module type TYPED_ARRAY = sig
  include JS_OBJECT

  val name : string

  val create : ?offset:Number.t -> ?length:Number.t -> ArrayBuffer.t -> t

  val bytes_per_element : Number.t

  val buffer : t -> ArrayBuffer.t

  val byte_offset : t -> Number.t

  val byte_length : t -> Number.t

  type elt

  val get_exn : t -> int -> elt

  val get_opt : t -> int -> elt option

  val set : t -> int -> elt -> t
end

(* https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Uint8Array *)
(* The name must be different than Uint8Array if name are kept, otherwise when
   compiled to JavaScript, Uin8Array will be resolved to this module.
 *)
module Uint8TypedArray = struct
  type elt = int

  type t = Js_of_ocaml.Typed_array.uint8Array Js.t

  let name = "Uint8Array"

  let of_js x =
    assert (
      Js.to_string (Js.Unsafe.get (Js.Unsafe.get x "constructor") "name") = name
    ) ;
    Js.Unsafe.coerce x

  let to_any_js x = Js.Unsafe.inject x

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])

  let bytes_per_element =
    Number.of_js (Js.Unsafe.get (Js.Unsafe.variable name) "BYTES_PER_ELEMENT")

  let buffer x = ArrayBuffer.of_js (Js.Unsafe.get x "buffer")

  let byte_length x = Number.of_js (Js.Unsafe.get x "byteLength")

  let byte_offset x = Number.of_js (Js.Unsafe.get x "byteOffset")

  let create ?(offset = Number.zero) ?length array_buffer =
    (* TODO: add checks on parameters *)
    let params =
      if Option.is_none length then
        [| ArrayBuffer.to_any_js array_buffer; Number.to_any_js offset |]
      else
        [| ArrayBuffer.to_any_js array_buffer;
           Number.to_any_js offset;
           Number.to_any_js (Option.get length)
        |]
    in
    (Js.Unsafe.new_obj (Js.Unsafe.variable name)) params

  let get_opt x (i : int) : elt option =
    Js.Optdef.to_option (Js_of_ocaml.Typed_array.get x i)

  let get_exn x i = Js_of_ocaml.Typed_array.unsafe_get x i

  let set x i elt =
    Js_of_ocaml.Typed_array.set x i elt ;
    x

  let to_bytes x =
    Bytes.of_string (Js_of_ocaml.Typed_array.String.of_uint8Array x)
end

(* https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Uint16Array *)
module Uint16TypedArray = struct
  type elt = int

  type t = Js_of_ocaml.Typed_array.uint16Array Js.t

  let name = "Uint16Array"

  let of_js x =
    assert (
      Js.to_string (Js.Unsafe.get (Js.Unsafe.get x "constructor") "name") = name
    ) ;
    Js.Unsafe.coerce x

  let to_any_js x = Js.Unsafe.inject x

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])

  let bytes_per_element =
    Number.of_js (Js.Unsafe.get (Js.Unsafe.variable name) "BYTES_PER_ELEMENT")

  let buffer x = ArrayBuffer.of_js (Js.Unsafe.get x "buffer")

  let byte_length x = Number.of_js (Js.Unsafe.get x "byteLength")

  let byte_offset x = Number.of_js (Js.Unsafe.get x "byteOffset")

  let create ?(offset = Number.zero) ?length array_buffer =
    (* TODO: add checks on parameters *)
    let params =
      if Option.is_none length then
        [| ArrayBuffer.to_any_js array_buffer; Number.to_any_js offset |]
      else
        [| ArrayBuffer.to_any_js array_buffer;
           Number.to_any_js offset;
           Number.to_any_js (Option.get length)
        |]
    in
    (Js.Unsafe.new_obj (Js.Unsafe.variable name)) params

  let get_opt x (i : int) : elt option =
    Js.Optdef.to_option (Js_of_ocaml.Typed_array.get x i)

  let get_exn x i = Js_of_ocaml.Typed_array.unsafe_get x i

  let set x i elt =
    Js_of_ocaml.Typed_array.set x i elt ;
    x
end

module ESModule = struct
  include Js_object_base

  (* let require name : t =
   *   Js.Unsafe.eval_string (Printf.sprintf "require('%s')" name) *)

  let call m fn_name args = Js.Unsafe.fun_call (Js.Unsafe.get m fn_name) args
end
