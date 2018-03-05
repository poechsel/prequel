type t = {i : int ref; msg : string }
type o = string

let open_feed s =
  { i = ref 0; msg = s }

let close_feed t = 
  ()


let next c = 
  if !(c.i) <= 5 then
    let _ = incr (c.i) in
    Some c.msg
  else 
    None

let reset c = 
  c.i := 0
