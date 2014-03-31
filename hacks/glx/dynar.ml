type 'a t = { mutable ar: 'a array; mutable n: int; mutable max_n: int }

let of_array ar =
  let n = Array.length ar in
  { ar=ar; n=n; max_n=n }

let push da v =
  if da.n < da.max_n
  then (da.ar.(da.n) <- v; da.n <- succ da.n)
  else begin
    let old_get = Array.unsafe_get da.ar in
    let old_max = da.max_n in
    let new_size = succ da.max_n * 2 in
    let new_ar = Array.init new_size (fun i -> if i < old_max then old_get i else v) in
    da.ar <- new_ar;
    da.n <- succ da.n;
    da.max_n <- new_size;
  end
;;

(* returns the index of the item that has been pushed *)
let pushi da v =
  let i = da.n in
  push da v;
  (i)

let to_array da =
  Array.init da.n (Array.unsafe_get da.ar)

