module Parser.Utils

module List = 
  let filter_map f xs = 
      xs
      |> List.map f
      |> List.filter (function Some x -> true | None -> false)      
      |> List.map    (function Some x -> x | None -> failwith "false") 

(*
module Array = 
  let filter_map f xs = 
      xs
      |> Array.map f
      |> Array.filter (function Some x -> true | None -> false)      
      |> Array.map    (function Some x -> x | None -> failwith "false") 
*)
