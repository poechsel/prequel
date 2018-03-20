let get_headers h_a h_b =
  h_a

(* this one is not totally inline, it would require an external sort *)
class union (left : AlgebraTypes.feed_interface) (right : AlgebraTypes.feed_interface) =
  object(self)
    inherit AlgebraTypes.feed_interface
    val left = left
    val right = right
    val hashtbl = Hashtbl.create 0
    
    method next = 
      let next = match left#next with
        | None -> right#next
        | x -> x
      in match next with
      | None -> None
      | Some x ->
        if Hashtbl.mem hashtbl x then
          self#next
        else 
          let _ = Hashtbl.add hashtbl x x in
          Some x

    method reset = 
      let _ = Hashtbl.reset hashtbl in 
      let _ = left#reset in 
      right#reset

    method headers =
      get_headers left#headers right#headers
  end
      
