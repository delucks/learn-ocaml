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

(*** Problem 6 ***)
let is_palindrome list =
  list = reverse_list list
;;

(*** Problem 7 ***)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;

(* this one confused me a bit; I wasn't clear on why I needed the [] case
 * or why both One and Many require the cons syntax to destructure the input list
 * Also, some testing confusion- I need to make sure to redeclare variables in the REPL that were declared in an earlier version of the module if I make changes when running #use "foo.ml";;
 *)
let flatten list =
  let rec inner_flatten acc = function
    | [] -> acc
    | One x :: tail -> inner_flatten (x :: acc) tail
    | Many x :: tail -> inner_flatten (inner_flatten acc x) tail in
  reverse_list (inner_flatten [] list)
;;

(*** Problem 8 ***)
(*
 * Compressing a list:
 * previous_item = Null value
 * result_list = [ ]
 * for item in list
 *   if item == previous_item
 *     skip
 *   else
 *     result_list append item
 *     previous_item = item
 *)
let compress list =
  let rec inner_compress last acc = function
    | [] -> acc
    | [x] -> if x != last then x :: acc else acc
    | head :: tail -> if head != last then (inner_compress head (head :: acc) tail) else acc in
  inner_compress "" [] list
;;
