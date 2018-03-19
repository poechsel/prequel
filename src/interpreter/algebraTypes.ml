type header = string * string

type expression =
  | AlgBinOp of Ast.binop * expression * expression
  | AlgAtom of Ast.atom

type algebra =
  | AlgUnion of algebra * algebra
  | AlgMinus of algebra * algebra
  | AlgProjection of algebra * header array
  | AlgInput of string (* for input nodes *)
  | AlgProduct of algebra * algebra
  | AlgSelect of algebra * expression
  | AlgRenameTable of algebra * string

type feed_result = string array

class virtual feed_interface =
  object(self)
    method virtual next : feed_result option
    method virtual headers : header array
    method virtual reset : unit
    method save (channel : out_channel) : unit =
      let rec aux () = 
        match self#next with
        | None -> ()
        | Some x -> 
          let s = Utils.array_concat ", " x in
          Printf.fprintf channel "%s\n" s;
          aux ()
      in let s = Utils.array_concat ", " (Array.map snd self#headers) in
      let _ = Printf.fprintf channel "%s\n" s in
      aux ()
  end
