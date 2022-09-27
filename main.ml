(* Last element of a list *)
let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last tl
;;

(* Last two elements of a list *)
let rec last_two l =
  match l with
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl
;;

(* Nth element of a list *)
let rec list_nth l n =
  let rec list_nth' l n c =
    match l with
    | [] -> None
    | h :: tl -> if n = c then Some h else list_nth' tl n (c + 1)
  in
  list_nth' l n 0
;;

let rec list_at l n =
  match l with
  | [] -> None
  | h :: tl -> if n = 0 then Some h else list_at tl (n - 1)
;;

(* List length *)
let rec list_length l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + list_length tl
;;

(* List length with tail-recursive *)
let rec list_length' l =
  let rec inner n = function
    | [] -> n
    | _ :: t -> inner (n + 1) t
  in
  inner 0 l
;;

(* List reverse tail-recursive *)
let rec list_reverse list =
  let rec inner acc = function
    | [] -> acc
    | h :: tl -> inner (h :: acc) tl
  in
  inner [] list
;;

let is_palindrome l = l = list_reverse l

(* Flatten list *)

type 'a node =
  | One of 'a
  | Many of 'a node list

(* Tail recursive *)
let rec flatten node =
  let rec inner acc = function
    | [] -> acc
    | One h :: tl -> inner (h :: acc) tl
    | Many h :: tl -> inner (inner acc h) tl
  in
  inner [] node
;;

let rec flatten' node =
  match node with
  | [] -> []
  | One h :: tl -> h :: flatten' tl
  | Many h :: tl -> flatten' h @ flatten' tl
;;

(* Compress (remove duplicates) *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller
;;
