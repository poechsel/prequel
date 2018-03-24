open Utils
open Lexing


exception SyntaxError of string
exception SemanticError of string
exception InterpretationError of string


(** make_syntax_error : Lexing.position -> string -> SyntaxError
    Creates an user-readable SyntaxError from lexing information. *)
let make_syntax_error infos token =
  let file =
    if infos.pos_fname = "" then
      "standard input"
    else
      "file " ^ infos.pos_fname in

  let message =
    if infos <> Lexing.dummy_pos then
      Printf.sprintf
        "%s line %d, character %d: error when seeing token `%s`."
        file
        infos.pos_lnum
        (1 + infos.pos_cnum - infos.pos_bol)
        token
    else
      token in

  SyntaxError message