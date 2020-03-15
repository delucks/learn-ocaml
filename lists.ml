(*** Problem 1 ***)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | head :: tail -> last tail
;;

(*** Problem 2 ***)
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

(*** Problem 3 ***)
let rec at k = function
  | [] -> None
  | head :: tail -> if k = 0 then Some head else at (k-1) tail
;;

(*** Problem 4 ***)
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
;;

(*** Problem 5 ***)
(* This needs an auxillary function because it must track state, namely the "acc"umulator list which keeps all reversed elements. This builds a call stack until the "tail" var is [], popping back up the call stack and appending an element to the acc list each time *)
let reverse_list list =
  let rec aux acc = function
    | [] -> acc
    | head :: tail -> aux (head :: acc) tail in
  aux [] list;;
;;
