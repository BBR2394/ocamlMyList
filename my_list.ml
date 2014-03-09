#!/usr/bin/ocaml

type 'a my_list = 
| Item of ('a * 'a my_list)
| Empty
;;

let hd  = function 
| Item(now, next) -> now 
| Empty -> failwith "hd"
;;

let tl = function
| Item(now, next) -> next
| Empty -> failwith "tl"
;;

let rec length_bis a  = function
| Item(now, next) -> length_bis (a+1) next
| _ -> a
;;
let length list =  length_bis 0 list;;

let rec nth_bis  n = function
| Item(now, next) -> if n = 0 
			then now
			else nth_bis (n-1) next
| Empty -> failwith "nth"
;;

let nth list n = nth_bis n list;;

let rec exist elem = function
| Item(now, next) -> elem now || exist elem next
| _ -> false
;;

let rec append list1 list2 = match list1 with
| Empty -> list2
| Item(now, next) -> (Item(now, append next list2))
;;

let rec rev_append list1 list2 = match list1 with
| Empty -> list2
| Item(now, next) -> rev_append next (Item(now, list2))
;;

let rev list = rev_append list Empty ;;

let rec flatten = function
| Empty -> Empty
| Item(curent, next) -> append curent (flatten next) ;;

let rec map fonc = function
| Empty -> Empty
| Item(curent, next) -> let res = fonc curent in Item(res, map fonc next);;

let rec iter fonc = function
| Empty -> ()
| Item(curent, next) -> fonc curent ; iter fonc next
;;

let  rec fold_left fonc accu list = match list with
| Empty -> accu
| Item(curent, next) -> fold_left fonc (fonc accu curent) list;;

let rec for_all p = function
| Empty -> true
| Item(curent, next) -> p curent && for_all p next;;

let rec mem param = function
| Empty -> false
| Item(curent, next) -> compare curent param = 0 || mem param next
;;

let rec memq param = function
| Empty -> false
| Item(curent, next) -> curent == param || memq param next;;

let rec split = function
| Empty -> (Empty, Empty)
| Item((x, y), next) -> let (x2, y2) = split next in (Item(x, x2), Item(y, y2));; 

let rec mem_assoc x = function
| Empty -> false
| Item((y, z), next) -> compare y x = 0 || mem_assoc x next;; 

let rec assoc x = function
| Empty -> failwith "Not_found"
| Item((a, b), next) -> if a == x
			then b
			else assoc x next
;;