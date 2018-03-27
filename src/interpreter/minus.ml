let get_headers h_a h_b = h_a

(* this one is not totally inline, it would require an external sort *)
class minus (left : AlgebraTypes.feed_interface) (right : AlgebraTypes.feed_interface) =
  object(self)
    inherit AlgebraTypes.feed_interface
    val left = left
    val right = right
    val hashtbl = Hashtbl.create 0
    val mutable initialized = false
    
    method next = 
      let _ = if not initialized then
        let _ = initialized <- true in
        let rec aux () = 
          match right#next with
          | None -> ()
          | Some x -> 
            Hashtbl.add hashtbl x x; aux ()
        in aux ()
      in 
      match left#next with
          | None -> None
          | Some x -> 
            if Hashtbl.mem hashtbl x then
              self#next
            else 
              Some x

    method reset = 
      let _ = initialized <- false in
      let _ = Hashtbl.reset hashtbl in 
      let _ = left#reset in 
      right#reset

    method headers =
      get_headers left#headers right#headers
  end
      

(* Using a sort to provide a more memory efficient minus *)
class minusJoin (left : AlgebraTypes.feed_interface) (right : AlgebraTypes.feed_interface) =
  let left, left_evaluator = 
    let h = left#headers in 
    let keys = Array.map (fun x -> 
        Arithmetics.compile_value h (AlgebraTypes.AlgAtom(Ast.Attribute x)),
        Ast.Asc) h in
    new ExternalSort.sort left keys,
    fun l -> Array.map (fun (k, _) -> k l) keys
  
  in
  let right, right_evaluator = 
    let h = right#headers in 
    let keys = Array.map (fun x -> 
        Arithmetics.compile_value h (AlgebraTypes.AlgAtom(Ast.Attribute x)),
        Ast.Asc) h in
    new ExternalSort.sort right keys,
    fun l -> Array.map (fun (k, _) -> k l) keys
  in

  object(self)
    inherit AlgebraTypes.feed_interface
    val left = left
    val right = right
    val left_evaluator = left_evaluator
    val right_evaluator = right_evaluator
    val mutable initialized = false
    val mutable current_left = None
    val mutable current_right = None
    
    method next = 
      let _ = if not initialized then
        let _ = current_left <- left#next in
        let _ = current_right <- right#next in
        initialized <- true
      in 

      let rec next_until_neq feed current = 
        let next = feed#next in
        if next = current then
          next_until_neq feed current
        else 
          next
      in 


      (* we can compare two evaluations by using
         pervasives.compare *)
      match current_left, current_right with
      | None, None -> None
      | None, Some x ->
        current_right <- right#next;
        Some x
      | Some x, None ->
        current_left <- left#next;
        Some x
      | Some x, Some y ->
        let x_v = left_evaluator x in
        let y_v = right_evaluator x in
        (* if x < y *)
        if Pervasives.compare x_v y_v < 0 then begin
          current_left <- next_until_neq left current_left;
          Some x
        end 
        else if Pervasives.compare x_v y_v > 0 then begin
          current_right <- next_until_neq right current_right;
          self#next
        end 
        else begin  
          current_left <- next_until_neq left current_left;
          current_right <- next_until_neq right current_right;
          self#next
        end 

    method reset = 
      initialized <- false;
      left#reset;
      right#reset

    method headers =
      get_headers left#headers right#headers
  end
      
