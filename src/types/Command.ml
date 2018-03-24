(** A top-level input. **)
type t =
  | Command of string * (string option)
  | Query of (AlgebraTypes.algebra Ast.cond, AlgebraTypes.algebra) Ast.query