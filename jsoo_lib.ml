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
