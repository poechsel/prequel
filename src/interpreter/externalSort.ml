let compare headers keys a b =
  let tbl_a = Arithmetics.Env.make headers a in
  let tbl_b = Arithmetics.Env.make headers b in
  let rec aux keys =
       match keys with
       | [] -> 0
       | x::tl ->
         let temp = Pervasives.compare (Arithmetics.execute_value x tbl_a) (Arithmetics.execute_value x tbl_b)
         in if temp = 0 then aux tl
         else temp
  in aux keys


class sort (sub: AlgebraTypes.feed_interface) (keys : AlgebraTypes.expression list) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val mutable initialized = false
    val mutable cache = []
    
    method next = 
      let _ = 
        if initialized = false then
          let _ = initialized <- true in
          let rec aux acc = 
            match sub#next with
            | None -> acc
            | Some x -> aux (x::acc)
          in 
          let headers = self#headers in

          let _ = cache <- aux [] in
          let _ = Printf.printf "size %d \n" @@ List.length cache in
          let _ = cache <- List.sort (compare headers keys) cache in
          Printf.printf "size %d \n" @@ List.length cache
      in 
      match cache with
      | [] -> None
      | x::tl -> let _ = cache <- tl in Some x

    method reset = 
      let _ = initialized <- false in 
      sub#reset

    method headers =
      sub#headers
  end
