module Bounds_fsm

open Fsm_parsing

let minlat = ref 1000.0
let maxlat = ref (-1000.0)
let minlon = ref 1000.0
let maxlon = ref (-1000.0)

let p_latlon name = p_str (name + "=\"") >>> p_float >>> p_char '"'
let low_letter c = c >= 'a' && c <='z'
let p_param =
  p_plus (p_pred low_letter) >>> p_str "=\"" >>> p_many (p_pred ((<>)'"')) >>> p_char '"'
  
let p_node_param =
     (p_latlon "lat" >>= fun () -> let lat = !Fsm_parsing.float_res in minlat := min !minlat lat; maxlat := max !maxlat lat)
 ||| (p_latlon "lon" >>= fun () -> let lon = !Fsm_parsing.float_res in minlon := min !minlon lon; maxlon := max !maxlon lon)
 ||| p_param

let p_ws = p_many (p_pred ((>=) ' '))
let p_endnode = (p_str "/>") ||| (p_all_until "</node>")
let p_node = p_str "<node" >>> p_many (p_ws >>> p_node_param) >>> p_endnode
let p_tag = p_char '<' >>> p_many (p_pred ((<>) '>')) >>> p_char '>'
let p_osm = p_many ((p_node ||| p_tag) >>> p_ws)
  
let do_process osm =
  let ok_action () =
    Printf.printf "fsm ok: lat=%f..%f lon=%f..%f\n"
      !minlat !maxlat !minlon !maxlon in
  let parse = prepare ok_action (fun () -> Printf.printf "fail\n") p_osm in
  parse osm 