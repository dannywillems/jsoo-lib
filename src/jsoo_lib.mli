open Js_of_ocaml

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
  (** A BigInt object *)
  type t

  (** Create a BigInt value from an integer *)
  val of_int : int -> t

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

  (** Returns a Js.Unsafe.any value representing the same value given *)
  val to_any_js : t -> Js.Unsafe.any

  val of_js : Js.Unsafe.any -> t

  (** Convert to [Unsigned.UInt64.t]. Unsafe *)
  val to_uint64 : t -> Unsigned.UInt64.t

  (** Convert to [int]. Unsafe *)
  val to_int : t -> int

  (** Convert to [Unsigned.UInt32.t]. Unsafe *)
  val to_uint32 : t -> Unsigned.UInt32.t
end

module Number : sig
  (** A Number object *)
  type t

  (** Create a Number value from an integer. The value is not verified to be in
  the interval allowed for a Number object *)
  val of_int : int -> t

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

  val of_js : Js.Unsafe.any -> t

  (** Returns a Js.Unsafe.any value representing the same value given *)
  val to_any_js : t -> Js.Unsafe.any

  (** Convert to [Unsigned.UInt64.t]. Unsafe *)
  val to_uint64 : t -> Unsigned.UInt64.t

  (** Convert to [int]. Unsafe *)
  val to_int : t -> int

  (** Convert to [Unsigned.UInt32.t]. Unsafe *)
  val to_uint32 : t -> Unsigned.UInt32.t
end
