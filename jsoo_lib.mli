val get_input_by_id : string -> Dom_html.inputElement Js.t
val get_p_by_id : string -> Dom_html.paramElement Js.t
val get_div_by_id : string -> Dom_html.divElement Js.t
val get_blockquote_by_id : string -> Dom_html.quoteElement Js.t
val get_button_by_id : string -> Dom_html.buttonElement Js.t
val get_link_by_id : string -> Dom_html.linkElement Js.t
val get_body_by_id : string -> Dom_html.bodyElement Js.t

val console_log : string -> unit
val alert : string -> unit

(* See http://www.w3schools.com/tags/tag_link.asp *)
module Head :
  sig
    (* FIXME: external lib for listing media_type *)
    type media_type =
      | Text_css
      | Text_js
      | No_media_type

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

    type cross_origin =
      | Anonymous
      | Use_credentials
      | Default_cross_origin (* Empty argument *)

    (* Charset, rev and target not implemented because deprecated in HTML5 *)
    val add_link :  ?media_type:media_type       ->
                    ?cross_origin:cross_origin  ->
                    ?language_code:string       -> (* FIXME: Sum type! *)
                    ?media_query:string         -> (* FIXME: Sum type! *)
                    rel:rel                     ->
                    string                      ->
                    unit

    val add_css_link : string -> unit

    val add_js_script : string -> unit
  end

module Body :
  sig
    val add_js_script : string -> unit
  end
