type 'a t = { mutable size : int; data : 'a array; comp : 'a -> 'a -> int}

let make n comp =
  let rec next_pow_two acc = 
    if acc < n then
      next_pow_two (2 * acc)
    else acc
  in 
  { size = 0; data = Array.make (next_pow_two n) (Obj.magic ()); comp = comp }

let parent i = (i-1) / 2

let left i = 2 * i + 1

let right i = 2 * i + 2

let swap heap i j =
  let temp = heap.data.(i) in
  let _ = heap.data.(i) <- heap.data.(j) in
  let _ = heap.data.(j) <- temp in
  ()

let rec down_heap heap i = 
  let smallest = i in
  let smallest = if left i < heap.size && heap.comp (heap.data.(left i)) (heap.data.(i)) < 0 then
      left i
    else 
      smallest
  in 
  let smallest = if right i < heap.size && heap.comp (heap.data.(right i)) (heap.data.(i)) < 0 then
      right i
    else 
      smallest
  in if smallest != i then
    let _ = swap heap i smallest in
    down_heap heap smallest

let rec up_heap heap i = 
  if i != 0 then
    let f = parent i in
    if heap.comp (heap.data.(f)) (heap.data.(i)) >= 0 then
      let _ = swap heap i f in
      up_heap heap f

let insert heap element = 
  let _ = heap.data.(heap.size) <- element in
  let _ = heap.size <- heap.size + 1 in 
  let _ = up_heap heap (heap.size-1) in
  ()

let get_size t = t.size


let pop heap = 
  let e = heap.data.(0) in
  let _ = heap.size <- heap.size - 1 in
  let _ = heap.data.(0) <- heap.data.(heap.size) in
  let _ = down_heap heap 0 in
  e

(* let dump heap = 
  for i = 0 to heap.size - 1 do
    Printf.printf "%d  " (let Ast.Number(x) :: _, _ = heap.data.(i) in x)
  done ;
  Printf.printf "\n" *)
