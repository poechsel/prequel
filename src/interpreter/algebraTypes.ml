type header = string * string

let string_of_header header = 
  let prefix = match fst header with
    | "" -> ""
    | x -> x ^ "."
  in prefix ^ snd header

type expression =
  | AlgBinOp of Ast.binop * expression * expression
  | AlgAtom of Ast.atom

type algebra =
  | AlgUnion of algebra * algebra
  | AlgMinus of algebra * algebra
  | AlgProjection of algebra * header list
  | AlgInput of string (* for input nodes *)
  | AlgProduct of algebra * algebra
  | AlgSelect of algebra * expression
  | AlgRenameTable of algebra * string

type feed_result = string list

class virtual feed_interface =
  object(self)
    method virtual next : feed_result option
    method virtual headers : header list
    method virtual reset : unit
    method save (channel : out_channel) : unit =
      let rec aux () = 
        match self#next with
        | None -> ()
        | Some x -> 
          let s = String.concat ", " x in
          Printf.fprintf channel "%s\n" s;
          aux ()
      in let s = String.concat ", " (List.map snd self#headers) in
      let _ = Printf.fprintf channel "%s\n" s in
      aux ()
  end
