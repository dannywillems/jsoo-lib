open Js_of_ocaml

module type JS_OBJECT = sig
  type t

  val of_js : Js.Unsafe.any -> t

  val to_any_js : t -> Js.Unsafe.any

  (** Equivalent to [toString] on the object in JavaScript *)
  val to_string : t -> string
end

(** Typed version of [getElementById], coercing to an input element *)
val get_input_by_id : string -> Dom_html.inputElement Js.t

(** Typed version of [getElementById], coercing to a paragraph element *)
val get_p_by_id : string -> Dom_html.paramElement Js.t

(** Typed version of [getElementById], coercing to a div element *)
val get_div_by_id : string -> Dom_html.divElement Js.t

(** Typed version of [getElementById], coercing to a blockquote element *)
val get_blockquote_by_id : string -> Dom_html.quoteElement Js.t

(** Typed version of [getElementById], coercing to a button element *)
val get_button_by_id : string -> Dom_html.buttonElement Js.t

(** Typed version of [getElementById], coercing to a link element *)
val get_link_by_id : string -> Dom_html.linkElement Js.t

(** Typed version of [getElementById], coercing to the body element *)
val get_body_by_id : string -> Dom_html.bodyElement Js.t

(** Typed version of [getElementById], coercing to an image element *)
val get_img_by_id : string -> Dom_html.imageElement Js.t

(** Typed version of [getElementById], coercing to a textarea element *)
val get_textarea_by_id : string -> Dom_html.textAreaElement Js.t

(** [console_log s] logs [s] to the console *)
val console_log : string -> unit

(** [alert s] creates an alert with the string [s] *)
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

  (** Add a css asset in the header *)
  val add_css_link : string -> unit

  (** Add a JavaScript script in the header *)
  val add_js_script : string -> unit
end

module Body : sig
  (** Add a JavaScript script in the body *)
  val add_js_script : string -> unit

  val append_child_mult : #Dom.node Js.t -> #Dom.node Js.t list -> unit
end

(** Binding to the BigInt class *)
module BigInt : sig
  include JS_OBJECT

  (** Create a [BigInt] value from an integer *)
  val of_int : int -> t

  val zero : t

  val one : t

  (** Create a [BigInt] value from a string *)
  val of_string : string -> t

  (** Create a [BigInt] value from a UInt64 value *)
  val of_uint64 : Unsigned.UInt64.t -> t

  (** Create a [BigInt] value from a UInt32 value *)
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

  (** Create a [Number] value from an integer. The value is not verified to be in
  the interval allowed for a [Number] object *)
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
