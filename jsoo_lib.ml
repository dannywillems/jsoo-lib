let doc = Dom_html.document

(* -------------------------------------------------------------------------- *)
let get_input_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.input
  (fun _ -> assert false)

let get_p_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.p
  (fun _ -> assert false)

let get_div_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.div
  (fun _ -> assert false)

let get_blockquote_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.blockquote
  (fun _ -> assert false)

let get_button_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.button
  (fun _ -> assert false)

let get_link_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.link
  (fun _ -> assert false)

let get_body_by_id str =
  Js.coerce (Dom_html.getElementById str) Dom_html.CoerceTo.body
  (fun _ -> assert false)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
let console_log str =
  Firebug.console##log (Js.string str)

let alert str =
  Dom_html.window##alert (Js.string str)
(* -------------------------------------------------------------------------- *)

module Head =
  struct
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

    let media_type_to_str s = match s with
      | Text_css -> "text/css"
      | Text_js -> "text/js"
      | No_media_type -> ""

    let rel_to_str s = match s with
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

    let cross_origin_to_str s = match s with
      | Anonymous -> "anonymous"
      | Use_credentials -> "use-credentials"
      | Default_cross_origin -> ""

    let add_link
      ?(media_type=No_media_type) ?(cross_origin=Default_cross_origin)
      ?(language_code="") ?(media_query="") ~rel href =
      let l = Dom_html.createLink doc in
      (*l##.crossorigin := Js.string (cross_origin_to_str cross_origin);*)
      l##.href := Js.string href;
      if language_code <> "" then l##.hreflang := Js.string (language_code);
      if media_query <> "" then l##.media := Js.string (media_query);
      l##.rel := Js.string (rel_to_str rel);
      l##._type := Js.string (media_type_to_str media_type);
      Dom.appendChild doc##.head l

    let add_css_link href =
      add_link ~media_type:Text_css ~rel:Stylesheet href

    let add_js_script l =
      let script = Dom_html.createScript doc in
      script##.src := Js.string l;
      script##._type := Js.string "text/javascript";
      Dom.appendChild doc##.head script
  end

module Body =
  struct
    let add_js_script l =
      let script = Dom_html.createScript doc in
      script##.src := Js.string l;
      script##._type := Js.string "text/javascript";
      Dom.appendChild doc##.body script
  end
