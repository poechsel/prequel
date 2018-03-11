(* this one is not totally inline, it would require an external sort *)
class minus (left : AlgebraTypes.feed_interface) (right : AlgebraTypes.feed_interface) =
  object(self)
    inherit AlgebraTypes.feed_interface
    val left = left
    val right = right
    val hashtbl = Hashtbl.create 0
    val mutable initialized = false
    
    method next = 
      let _ = if not initialized then
        let _ = initialized <- true in
        let rec aux () = 
          match right#next with
          | None -> ()
          | Some x -> Hashtbl.add hashtbl x x; aux ()
        in aux ()
      in 
      match left#next with
          | None -> None
          | Some x -> 
            if Hashtbl.mem hashtbl x then
              self#next
            else 
              Some x

    method reset = 
      let _ = left#reset in 
      right#reset

    method headers =
      let _ = initialized <- false in
      let _ = Hashtbl.reset hashtbl in 
      left#headers
  end
      
