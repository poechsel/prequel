let build_rename_map rename h =
  let tbl = Hashtbl.create (List.length rename) in
  let () = List.iter (fun (key, value) -> 
      Hashtbl.add tbl key value) rename in
  tbl


let get_headers tbl h = 
  Array.map (fun i ->
      if Hashtbl.mem tbl i then 
        Hashtbl.find tbl i
      else 
        i
    ) h

let inverse_rename rename_list h =
  h
  |> List.map (fun attribute ->
      let s = Utils.find_first 
          (fun (before, after) -> after = attribute ) 
          rename_list in
      match s with
      | None -> attribute
      | Some (x, _) -> x
    )


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
