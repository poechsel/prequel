let build_rename_map rename h =
  let tbl = Hashtbl.create (List.length rename) in
  let () = List.iter (fun (key, value) -> 
      
  let _ = Printf.printf "-> %s %s\n" (Debug.string_of_header key) (Debug.string_of_header value) in
      Hashtbl.add tbl key value) rename in
  tbl


let get_headers tbl h = 
  Array.map (fun i ->
      Printf.printf "[%s]\n" @@ Debug.string_of_header i ;
      if Hashtbl.mem tbl i then 
        Hashtbl.find tbl i
      else 
        let _ = Printf.printf "o\n" in
        i
    ) h

(* renamed is of type (header * header) list 
   For each tuples, the first coordinate is the name of the row to be renamed
   and the second the new name
*)
class rename (sub : AlgebraTypes.feed_interface) ( renamed : (AlgebraTypes.header * AlgebraTypes.header) list ) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val rename_hashtbl = build_rename_map renamed sub#headers
    val sub = sub

    method next = sub#next

    method reset =
      sub#reset

    method headers =
      get_headers rename_hashtbl sub#headers
  end 
