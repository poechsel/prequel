let get_headers h name =
    Array.append h [|("", name)|]

class addColumn (sub : AlgebraTypes.feed_interface) (expr : AlgebraTypes.feed_result -> Ast.atom) (name : string) =
  object (self)
    inherit AlgebraTypes.feed_interface

    val expr = expr
    val name = name

    method next =
      match sub#next with
      | None ->
        None
      | Some x ->
        let s = 
          match expr x with 
          | Ast.Number x    -> string_of_int x
          | Ast.String s    -> s
          | Ast.Attribute _ -> failwith "There shouldn't be an attribute here."
        in 
        Some (Array.append x [|s|])

    method reset = 
      sub#reset

    method headers = 
      get_headers sub#headers name
        
  end 
