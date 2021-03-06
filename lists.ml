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
(* Rewritten after looking at the solution, here's what I learned.
 * You don't need to track state if you're returning a type that's exactly what you recieve
 * Pattern matching is so expressive for binding variables and establishing state: from the solution,
 *     a :: (b :: _ as t)
 * this binds three variables, the first element, second element, and the second+tail elements all in one expression.
 * In doing so, it also avoids the additional cons that I incur calling "compress (second :: tail)" in my solution.
 * The "smaller -> smaller" notation used below is a catch-all case, not some kind of comparison.
 *)
let rec compress = function
  | first :: second :: tail -> if first = second then compress (second :: tail) else first :: compress (second :: tail)
  | smaller -> smaller
;;

(*** Problem 9 ***)
(* Here, my intuition to use match clause guards was correct, but I got confused by where to cons "b" in the form
 * a :: b :: tail, so I changed it to use the "as" assignment pattern like the previous question. The assignment
 * within match blocks is a useful shorthand to avoid this kind of positional space confusion.
 *)
let pack list =
  let rec inner_pack curr acc = function
    | a :: (b :: _ as tail) ->
        if a = b then inner_pack (a :: curr) acc tail
        else inner_pack [] ((a :: curr) :: acc) tail
    | [x] -> (x :: curr) :: acc
    | [] -> [] in
  reverse_list (inner_pack [] [] list)
;;

(*** Problem 10 ***)
let run_length_encode list =
  let rec rle_aux count acc = function
    | a :: (b :: _ as tail) ->
        if a = b then rle_aux (count+1) acc tail
        else rle_aux 0 ((count+1, a) :: acc) tail
    | [x] -> (count+1, x) :: acc
    | [] -> [] in
  reverse_list (rle_aux 0 [] list)
;;

(*** Problem 11 ***)
type 'a run_length_encoded =
  | One of 'a
  | Many of int * 'a
;;

let rle list =
  let pack_run_length_encoded count value =
    if count = 0 then One value
    else Many (count+1, value) in
  let rec rle_aux count acc = function
    | a :: (b :: _ as tail) ->
        if a = b then rle_aux (count+1) acc tail
        else rle_aux 0 ((pack_run_length_encoded count a) :: acc) tail
    | [x] -> (pack_run_length_encoded count x) :: acc
    | [] -> [] in
  reverse_list (rle_aux 0 [] list)
;;

(*** Problem 12 ***)
let decode_rle list =
  let rec construct_n_list count value acc =
    match count with
    | 0 -> acc
    | _ -> construct_n_list (count-1) value (value :: acc) in
  let rec decode_aux acc = function
    | head :: tail ->
        (match head with
          | One x -> decode_aux (x :: acc) tail
          | Many (ct, x) -> decode_aux ((construct_n_list ct x []) @ acc) tail)
    | [] -> acc in
  reverse_list (decode_aux [] list)
;;

(* Problem 13 is confusing because it's asking for an equivalent implementation to my #11... so going to skip that *)

(*** Problem 14 ***)
let duplicate list =
  let rec duplicate_aux acc = function
    | head :: tail -> duplicate_aux (head :: head :: acc) tail
    | [] -> acc in
  reverse_list (duplicate_aux [] list)
;;
