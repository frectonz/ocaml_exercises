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
let rec list_length' l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + list_length' tl
;;

(* List length with tail-recursive *)
let list_length l =
  let rec inner n = function
    | [] -> n
    | _ :: t -> inner (n + 1) t
  in
  inner 0 l
;;

(* List reverse tail-recursive *)
let list_reverse list =
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
let rec compress' = function
  | a :: (b :: _ as t) -> if a = b then compress' t else a :: compress' t
  | smaller -> smaller
;;

(* Compress tail-recursive *)
let compress lst =
  let rec inner acc = function
    | [] -> []
    | a :: [] -> a :: acc
    | a :: (b :: _ as t) -> if a = b then inner acc t else inner (a :: acc) t
  in
  inner [] lst |> List.rev
;;

(* Scanl *)
let rec scanl' f acc l =
  match l with
  | [] -> [ acc ]
  | x :: tl -> acc :: scanl' f (f acc x) tl
;;

let scanl f acc l =
  let rec inner' acc_list f acc l =
    match l with
    | [] -> acc_list
    | x :: tl -> inner' (acc :: acc_list) f (f acc x) tl
  in
  inner' [] f acc l |> List.rev
;;

let rec scanl1' f l =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: tl -> x :: scanl1' f (f x y :: tl)
;;

let rec scanl1 f l =
  let rec inner' acc_list f l =
    match l with
    | [] -> acc_list
    | [ x ] -> x :: acc_list
    | x :: y :: tl -> inner' (x :: acc_list) f (f x y :: tl)
  in
  inner' [] f l |> List.rev
;;

(*
   # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
   [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] *)
let pack lst =
  let rec inner acc acc' = function
    | [] -> acc
    | [ a ] -> (a :: acc') :: acc
    | a :: (b :: _ as t) ->
      let acc' = a :: acc' in
      if a = b then inner acc acc' t else inner (acc' :: acc) [] t
  in
  inner [] [] lst |> List.rev
;;

(*
   # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   [("a", 4); ("b", 1); ("c", 2); ("a", 2); ("d", 1); ("e", 4)] *)
let encode lst =
  let rec inner acc counter = function
    | [] -> acc
    | [ a ] -> (a, counter + 1) :: acc
    | a :: (b :: _ as t) ->
      let counter = counter + 1 in
      if a = b then inner acc counter t else inner ((a, counter) :: acc) 0 t
  in
  inner [] 0 lst |> List.rev
;;

type 'a rle =
  | One of 'a
  | Many of 'a * int

(* encode_rle ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   [Many ("a", 4); One "b"; Many ("c", 2); Many ("a", 2); One "d"; Many ("e", 4)] *)
let encode_rle lst =
  encode lst
  |> List.map (function
    | a, 1 -> One a
    | a, x -> Many (a, x))
;;

(* decode [Many ("a", 4); One "b"; Many ("c", 2); Many ("a", 2); One "d"; Many ("e", 4)];;
   ["a"; "a"; "a"; "a"; "a"; "c"; "c"; "a"; "a"; "a"; "e"; "e"; "e"; "e"] *)
let decode lst =
  lst
  |> List.map (function
    | One a -> [ "a" ]
    | Many (a, x) -> List.init x (fun _ -> a))
  |> List.concat
;;

(* duplicate ["a"; "b"; "c"; "c"; "d"];;
   ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let duplicate lst =
  let rec inner acc = function
    | [] -> acc
    | h :: t -> inner (h :: h :: acc) t
  in
  inner [] lst |> List.rev
;;

let rec duplicate' = function
  | [] -> []
  | h :: t -> h :: h :: duplicate' t
;;

(* replicate ["a"; "b"; "c"] 3;;
   ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
let replicate lst repeat =
  lst |> List.map (fun a -> List.init repeat (fun _ -> a)) |> List.concat
;;

(* drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
   ["a"; "b"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] *)
let drop lst n =
  let rec inner acc count n = function
    | [] -> acc
    | h :: t ->
      let acc = if count = n then acc else h :: acc in
      inner acc (count + 1) n t
  in
  inner [] 1 n lst |> List.rev
;;

(* split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
   [["a"; "b"; "c"]; ["d"; "e"; "f"; "g"; "h"; "i"; "j"]] *)
let split lst n =
  let rec inner acc acc' n = function
    | [] -> acc
    | h :: t ->
      if List.length acc' = n
      then inner ((h :: t) :: (acc' |> List.rev) :: acc) [] n []
      else inner acc (h :: acc') n t
  in
  inner [] [] n lst |> List.rev
;;

(* slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
   ["c"; "d"; "e"; "f"; "g"] *)
let slice lst s e =
  let rec inner acc count s e = function
    | [] -> acc
    | h :: t ->
      if count >= s && count <= e
      then if count = e then h :: acc else inner (h :: acc) (count + 1) s e t
      else inner acc (count + 1) s e t
  in
  inner [] 0 s e lst |> List.rev
;;

(* rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
   ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)
let rotate lst n = split lst n |> List.rev |> List.concat

(* remove_at 1 ["a"; "b"; "c"; "d"];;
   ["a"; "c"; "d"] *)
let remove_at n lst =
  let rec inner acc count n = function
    | [] -> acc
    | h :: t ->
      if count = n then inner acc (count + 1) n t else inner (h :: acc) (count + 1) n t
  in
  inner [] 0 n lst |> List.rev
;;

(* insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
   ["a"; "alfa"; "b"; "c"; "d"] *)
let insert_at a p lst =
  let rec inner acc count a p = function
    | [] -> acc
    | h :: t ->
      if count = p
      then inner (h :: a :: acc) (count + 1) a p t
      else inner (h :: acc) (count + 1) a p t
  in
  inner [] 0 a p lst |> List.rev
;;

(* range 4 9;;
   [4; 5; 6; 7; 8; 9] *)
let range s e =
  let rec inner acc s e = if s > e then acc else inner (s :: acc) (s + 1) e in
  inner [] s e |> List.rev
;;

let rec range' s e = if s > e then [] else s :: range' (s + 1) e

let rand_select n lst =
  let rec inner acc =
    let rand = List.nth lst (Random.int (List.length lst)) in
    let already_exists = List.exists (fun a -> a = rand) acc in
    if List.length acc = n
    then acc
    else if already_exists
    then inner acc
    else inner (rand :: acc)
  in
  inner []
;;

let lotto_select n m = range 1 m |> rand_select n
let permutation lst = rand_select (List.length lst) lst
