open Utils

type header = Ast.attribute
[@@deriving show]

let uid = ref 0
let new_uid () = incr uid; !uid

type uid = int
[@@deriving show]

type expression =
  | AlgBinOp of Ast.binop * expression * expression
  | AlgAtom of Ast.atom
[@@deriving show]

type algebra =
  | AlgUnion      of uid * algebra * algebra
  | AlgMinus      of uid * algebra * algebra
  | AlgProjection of uid * algebra * header array
  | AlgJoin       of uid * (algebra * expression) * (algebra * expression)
  | AlgInput      of uid * string (* for input nodes *)
  | AlgProduct    of uid * algebra * algebra
  | AlgSelect     of uid * algebra * expression
  | AlgRename     of uid * algebra * (header * header) list
  | AlgAddColumn  of uid * algebra * expression * string
  | AlgOrder      of uid * algebra * (expression * Ast.ordering) array
  | AlgGroup      of uid * algebra * expression array * (string * (Ast.aggop * header)) array
[@@deriving show]

type feed_result = string array

class virtual feed_interface =
  object(self)
    method virtual next : feed_result option
    method virtual headers : header array
    method virtual reset : unit

    method to_list : feed_result list =
      let rec aux acc = 
        match self#next with
        | None -> 
          List.rev acc
        | Some x ->
          aux (x::acc)
      in aux []

    method iterate (fct : string array -> unit) : unit =
      let rec aux () = 
        match self#next with
        | None -> 
          ()
        | Some x ->
          fct x;
          aux ()
      in aux ()

    method save (channel : out_channel) : unit =
      let rec aux () = 
        match self#next with
        | None -> ()
        | Some x -> 
          let s = array_concat ", " x in
          Printf.fprintf channel "%s\n" s;
          aux ()
      in let s = array_concat ", " (Array.map snd self#headers) in
      let _ = Printf.fprintf channel "%s\n" s in
      aux ()

    method print : unit =
      Utils.print_table @@ (Array.map snd self#headers) :: self#to_list
  end


let attributes_of_condition cond =
  let rec aux c acc =
    match c with
    | AlgBinOp(_, a, b) ->
      aux a acc
      |> aux b
    | AlgAtom (Ast.Attribute x) ->
      x :: acc
    | _ ->
      acc
  in aux cond [] 
     |> List.sort_uniq Pervasives.compare 


