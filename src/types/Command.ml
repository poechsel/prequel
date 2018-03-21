(** A top-level input. **)
type t =
  | Command of string
  | Query of (AlgebraTypes.algebra Ast.cond, AlgebraTypes.algebra) Ast.query