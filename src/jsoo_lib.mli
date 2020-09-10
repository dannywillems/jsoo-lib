open Js_of_ocaml

module type JS_OBJECT = sig
  type t

  val of_js : Js.Unsafe.any -> t

  val to_any_js : t -> Js.Unsafe.any

  val to_string : t -> string
end

val get_input_by_id : string -> Dom_html.inputElement Js.t

val get_p_by_id : string -> Dom_html.paramElement Js.t

val get_div_by_id : string -> Dom_html.divElement Js.t

val get_blockquote_by_id : string -> Dom_html.quoteElement Js.t

val get_button_by_id : string -> Dom_html.buttonElement Js.t

val get_link_by_id : string -> Dom_html.linkElement Js.t

val get_body_by_id : string -> Dom_html.bodyElement Js.t

val get_img_by_id : string -> Dom_html.imageElement Js.t

val get_textarea_by_id : string -> Dom_html.textAreaElement Js.t

val console_log : string -> unit

val alert : string -> unit

val onload : (Dom_html.event Js.t -> bool Js.t) -> unit

(* See http://www.w3schools.com/tags/tag_link.asp *)
module Head : sig
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

  (* Charset, rev and target not implemented because deprecated in HTML5 *)
  val add_link :
    ?media_type:media_type ->
    ?cross_origin:cross_origin ->
    ?language_code:string ->
    (* FIXME: Sum type! *)
    ?media_query:string ->
    (* FIXME: Sum type! *)
    rel:rel ->
    string ->
    unit

  val add_css_link : string -> unit

  val add_js_script : string -> unit
end

module Body : sig
  val add_js_script : string -> unit

  val append_child_mult : #Dom.node Js.t -> #Dom.node Js.t list -> unit
end

(** Binding to the BigInt class *)
module BigInt : sig
  include JS_OBJECT

  (** Create a BigInt value from an integer *)
  val of_int : int -> t

  val zero : t

  val one : t

  (** Create a BigInt value from a string *)
  val of_string : string -> t

  (** Create a BigInt value from a UInt64 value *)
  val of_uint64 : Unsigned.UInt64.t -> t

  (** Create a BigInt value from a UInt32 value *)
  val of_uint32 : Unsigned.UInt32.t -> t

  (** [is_bigint js_obj] returns [true] if [js_obj] is of type [BigInt], else
      [false]
  *)
  val is_bigint : Js.Unsafe.any -> bool

  (** Convert to [Unsigned.UInt64.t]. Unsafe *)
  val to_uint64 : t -> Unsigned.UInt64.t

  (** Convert to [int]. Unsafe *)
  val to_int : t -> int

  (** Convert to [Unsigned.UInt32.t]. Unsafe *)
  val to_uint32 : t -> Unsigned.UInt32.t
end

module Number : sig
  include JS_OBJECT

  (** Create a Number value from an integer. The value is not verified to be in
  the interval allowed for a Number object *)
  val of_int : int -> t

  val zero : t

  val one : t

  (** Create a [Number] value from a string. The value is not verified to be in
  the interval allowed for a Number object *)
  val of_string : string -> t

  (** [is_number js_obj] returns [true] if [js_obj] is of type [Number], else
      [false]
   *)
  val is_number : Js.Unsafe.any -> bool

  (** Create a [Number] value from a [UInt64] value.
      The value is not verified to be in the interval allowed for a Number
      object.
   *)
  val of_uint64 : Unsigned.UInt64.t -> t

  (** Create a [Number] value from a [UInt32] value *)
  val of_uint32 : Unsigned.UInt32.t -> t

  (** Convert to [Unsigned.UInt64.t]. Unsafe *)
  val to_uint64 : t -> Unsigned.UInt64.t

  (** Convert to [int]. Unsafe *)
  val to_int : t -> int

  (** Convert to [Unsigned.UInt32.t]. Unsafe *)
  val to_uint32 : t -> Unsigned.UInt32.t
end

module ArrayBuffer : sig
  include JS_OBJECT

  val make : int -> char -> t

  val is_array_buffer : Js.Unsafe.any -> bool

  val length : t -> Number.t
end

module type TYPED_ARRAY = sig
  include JS_OBJECT

  (** OCaml type to represent the elements of the typed array. *)
  type elt

  (** Name of the typed array. Example: [Uint8Array], [Uint16Array] *)
  val name : string

  val create : ?offset:Number.t -> ?length:Number.t -> ArrayBuffer.t -> t

  (** [bytes_per_element a] returns the number of bytes an element of the typed array takes. Equivalent to
      [a.bytes_per_element] in JavaScript *)
  val bytes_per_element : Number.t

  (** [buffer a] returns the underlying buffer of the array. Equivalent to
      [a.buffer] in JavaScript *)
  val buffer : t -> ArrayBuffer.t

  (** Equivalent to [a.byteOffset] in JavaScript *)
  val byte_offset : t -> Number.t

  (** [byte_length a] returns the number of bytes of the typed array. Equivalent to [a.byteLength] in JavaScript *)
  val byte_length : t -> Number.t

  (** [get_exn a i] returns the element at position [i] in the array [a]. If [i]
      is greater than the length of the array, raises an exception in JavaScript.
      Equivalent to a[i] in JavaScript.
  *)
  val get_exn : t -> int -> elt

  (** [get_opt a i] returns the element at position [i] in the array [a] as an
      option. If [i] is greater than the length of the array, returns [None].
      Equivalent to a[i] in JavaScript.
  *)
  val get_opt : t -> int -> elt option

  (** [set a i x] is equivalent to a[i] = x in JavaScript *)
  val set : t -> int -> elt -> t

  (** [slice array a b] returns [array[a], ..., array[b]] *)
  val slice : t -> int -> int -> t
end

(** Binding to Uint8Array.
    Compatible with >= ES6 only
 *)
module Uint8TypedArray : sig
  include TYPED_ARRAY with type elt = int

  (** Convert to bytes *)
  val to_bytes : t -> bytes
end

(** Binding to Uint16Array.
    Compatible with >= ES6 only
 *)
module Uint16TypedArray : TYPED_ARRAY with type elt = int

(** Represent a ES module. Use [ESModule.of_js m] where [m] is [Js.Unsafe.js_expr {|require ("moule_name") |}] *)
module ESModule : sig
  include JS_OBJECT

  (** FIXME: how to provide a function for require? *)

  (* val require : string -> t *)

  val call : t -> string -> Js.Unsafe.any array -> Js.Unsafe.any
end

module Promise : sig
  type ('a, 'b) t

  val of_any_js : Js.Unsafe.any -> ('a, 'b) t

  type 'a fn = 'a -> unit

  val make : ('a fn -> 'b fn -> unit) -> ('a, 'b) t

  val then_bind : on_resolved:'a fn -> ?on_rejected:'b fn -> ('a, 'b) t -> unit

  val catch_bind : 'b fn -> ('a, 'b) t -> unit

  val finally_bind : unit fn -> (_, _) t -> unit
end
