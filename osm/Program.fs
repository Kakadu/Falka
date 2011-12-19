open Printf

let filename = @"../../../map.osm"
let content = System.IO.File.ReadAllText filename

let () = printf "combinators \n"
let () = Bounds_pc.do_process content

let () = printf "FSM \n"
let () = Bounds_fsm.do_process content
