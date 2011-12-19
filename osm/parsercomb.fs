#light 
module ParserComb
open Utils

type 'a parse_result = Parsed of 'a * char list | Failed

let retrn x s = Parsed(x, s)
let fail s = Failed

let p_pred p s =
  match s with    
  | [] -> Failed
  | h::t -> if p h then Parsed(h, t) else Failed
 
let p_char c = p_pred ((=) c)

let p_somechar s = 
  match s with
  | [] -> Failed
  | h::t -> Parsed(h, t)

let p_manyf prs f v0 =
  let rec loop v st =
    match prs st with
    | Parsed(x, s') -> loop (f v x) s'
    | Failed -> Parsed(v, st) in
  loop v0

let p_many prs  =
  let rec loop st =
    match prs st with
    | Parsed(_, s') -> loop s'
    | Failed -> Parsed((), st) in
  loop 

let p_opt defval p s =
  match p s with
  | Parsed _ as ok -> ok
  | Failed -> Parsed(defval, s)

let (|||) p1 p2 s =
  match p1 s with
  | Parsed _ as ok -> ok
  | Failed -> p2 s

let (>>=) p1 f s =
  match p1 s with
  | Parsed(x, s2) -> f x s2
  | Failed -> Failed

let (>>>) p1 p2 s =
  match p1 s with
  | Parsed(_, s2) -> p2 s2
  | Failed -> Failed

let p_plus prs = prs >>> p_many prs

let p_many1f prs f =
  prs >>= fun v0 ->  p_manyf prs f v0

let isdigit c = c>='0' && c<='9'
let p_digit = p_pred isdigit

let int_of_char (c: char) = 
    (int c) - (int 'a') + 1

let mkInt v x = v * 10 +  int_of_char x - 48

let p_int s =
  match s with
  | [] -> Failed
  | '-'::t -> (match p_manyf p_digit mkInt 0 t with
              | Parsed(x, s') -> Parsed(-x, s')
              | Failed -> Failed)
  | c :: _  when c>'0' && c<'9' -> p_manyf p_digit mkInt 0 s
  | _ -> Failed
  
let p_str str =
  String.fold_left (fun p c -> p >>> p_char c) (retrn '!') str;;

let rec p_seq prs = (* sequence of something *)
  prs >>= fun x ->
  p_opt [x] (p_seq prs >>= fun lst -> retrn (x::lst))

let rec p_list prs psep = (* list of something, separated by given separator parser *)
  prs >>= fun x ->
  p_opt [x] (psep >>> p_list prs psep >>= fun lst -> retrn (x::lst))

let rec p_listch prs sep = (* list of something, separated by given char *)
  prs >>= fun x ->
  p_opt [x] (p_char sep >>> p_listch prs sep >>= fun lst -> retrn (x::lst))

let p_intlist = p_listch p_int;;

let p_void prs s = 
  match prs s with
  | Parsed(_, s') -> Parsed((), s')
  | Failed -> Failed

let mkFloat (fv,fr) c = fv + float (int_of_char c - 48) * fr , fr * 0.1

let p_float =
  p_opt 1.0 (p_char '-' >>> retrn (-1.0)) >>= fun sign ->
  p_manyf p_digit mkInt 0 >>= fun n ->
  p_char '.' >>>
  p_manyf p_digit mkFloat (0.0, 0.1) >>= fun (fv, _) -> 
  retrn (sign * (float n + fv))

let p_all_until (str : string) =
  let ps = p_str str in
  let rec loop lst =
    match lst with   
    | [] -> Failed
    | c::tl -> 
        if c=str.[0] then
          match ps lst with
          | Parsed _ as ok  -> ok
          | Failed -> loop tl
        else loop tl in
  loop


