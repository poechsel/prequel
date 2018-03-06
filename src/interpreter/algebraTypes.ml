type header = string option * string


type expression =
  | AlgBinOp of Ast.binop * expression * expression
  | AlgAtom of Ast.atom

type algebra =
  | AlgUnion of algebra * algebra
  | AlgProjection of algebra * header list
  | AlgInput of string (* for input nodes *)
  | AlgSelect of algebra * expression

type feed_result = string list

module type Feed = sig
  type t
  type o
  val open_feed : o -> t
  val close_feed : t -> unit
  val reset : t -> unit
  val next : t -> (feed_result option) 
  val headers : t -> header list 
end

module type FeedHandlerInterface = sig
  module FeedHandler : Feed
  val this : FeedHandler.t
end

class virtual feed_interface =
  object
    method virtual next : feed_result option
    method virtual headers : header list
    method virtual reset : unit
  end


open Ast

let rec execute_value expr env =
  match expr with
  | AlgAtom y -> begin
      match y with
      | Attribute y ->
        String (Hashtbl.find env y)
      | _ -> y
    end
  | AlgBinOp(op, r, l) ->
      let fct = match op with
        | Add -> (+)
        | Sub -> (-)
        | Times -> ( * )
        | Div -> (/)
        | _ -> failwith "unexpected"
      in 
      let r = match (execute_value r env) with
        | String x -> int_of_string x
        | Number x -> x
      in
      let l = match (execute_value l env) with
        | String x -> int_of_string x
        | Number x -> x
      in Number (fct r l)


let alg_expr_of_ast_expr expr = 
  let rec c_cond cond = 
    match cond with
    | AstBinOp(op, a, b) ->
      AlgBinOp(op, c_cond a, c_cond b)
    | AstCompOp(op, a, b) ->
      AlgBinOp(op, c_expr a, c_expr b)
  and c_expr expr = 
    match expr with
    | AstExprOp(op, a, b) ->
      AlgBinOp(op, c_expr a, c_expr b)
    | AstAtom a ->
      AlgAtom a
  in c_cond expr

    

let rec execute_filter expr env =
  match expr with
  | AlgBinOp(op, r, l) -> 
    begin match op with
      | And ->
        execute_filter r env && execute_filter l env
      | Or ->
        execute_filter r env || execute_filter l env
      | Eq ->
        execute_value r env = execute_value l env
      | Neq ->
        execute_value r env != execute_value l env
      | Leq ->
        execute_value r env <= execute_value l env
      | Geq ->
        execute_value r env >= execute_value l env
      | Lt ->
        execute_value r env < execute_value l env
      | Gt ->
        execute_value r env > execute_value l env
      | _ -> failwith "unexpected"
    end 
  | _ -> failwith "unexpected"
