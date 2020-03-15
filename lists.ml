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


(*** Problem 4 ***)
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
