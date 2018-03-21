let (<|) = (@@)

let array_concat s a = 
  String.concat s (Array.to_list a)

let array_find el ar =
  let rec aux i = 
    if i = Array.length ar then
      failwith "element not found"
    else
    if ar.(i) = el then
      i
    else aux (i+1)
  in aux 0

let merge_list fct l = 
  if List.length l = 1 then 
    List.hd l
  else 
    List.fold_left (fun a b ->
        fct a b
      ) (List.hd l) (List.tl l)

let option_map fct x = 
  match x with
  | None -> None
  | Some x -> Some (fct x)


(** length_utf8 : string -> int
    Returns the length of an UTF8-encoded string. *)
let length_utf8 s =
  let rec length_aux s c i =
  if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4 in
    length_aux s (c + 1) (i + k) in
  length_aux s 0 0


(** print_table : string array list -> unit
    Pretty-prints a given table of strings. *)
let print_table m =
  let width = 20 in
  let cells = Array.length <| List.hd m in
  let delim = String.make ((width + 3) * cells + 1) '-' in

  let print_cell cell =
    let content =
      if String.length cell > width then
        (String.sub cell 0 (width - 3)) ^ "..."
      else
        cell in

    let pad = String.make (width - (length_utf8 content)) ' ' in
    Printf.printf "| %s%s " content pad in

  let print_header line =
    print_endline delim;
    Array.iter print_cell line;
    Printf.printf "|";
    print_newline ();
    print_endline delim in

  let print_line line =
    Array.iter print_cell line;
    Printf.printf "|";
    print_newline () in

  List.hd m |> print_header;
  List.tl m |> List.iter print_line;
  print_endline delim