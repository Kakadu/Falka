module Bounds_pc
#nowarn "62"
open ParserComb
open Utils

let p_latlon name =
  p_str (name ^ "=\"") >>> p_float >>= fun f -> p_char '"' >>> retrn f

let low_letter c = c >= 'a' && c <='z' 

let p_param = 
  p_plus (p_pred low_letter) >>> p_str "=\"" >>> p_many (p_pred ((<>)'"')) >>> p_char '"' >>> retrn ()
  
let minlat = ref 1000.0  
let maxlat = ref (-1000.0)  
let minlon = ref 1000.0  
let maxlon = ref (-1000.0)  
  
let p_node_param =
      (p_latlon "lat" >>= fun lat -> minlat := min !minlat lat; maxlat := max !maxlat lat; retrn ())  
  ||| (p_latlon "lon" >>= fun lon -> minlon := min !minlon lon; maxlon := max !maxlon lon; retrn ())  
  ||| p_param

let p_ws = p_many (p_pred ((>=) ' '))
let p_endnode = (p_str "/>") ||| (p_all_until "</node>")  
let p_node = p_str "<node" >>>  p_many (p_ws >>> p_node_param) >>> p_endnode
let p_tag = p_char '<' >>> p_many (p_pred ((<>) '>')) >>> p_char '>'
let p_osm = p_many ((p_node ||| p_tag) >>> p_ws)
  
let do_process osm = 
  let chars = String.explode osm in
  match p_osm chars with
  | Parsed(_, s) -> Printf.printf "pc ok: lat=%f..%f lon=%f..%f\n" !minlat !maxlat !minlon !maxlon 
  | Failed -> Printf.printf "parse failed\n"       


