module type Bag = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  val add : 'a -> 'a t -> 'a t
  val remove : 'a -> 'a t -> 'a t
  val contains : 'a -> 'a t -> bool

  val filter : ('a -> bool) -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val union : 'a t -> 'a t -> 'a t
  val intersection : 'a t -> 'a t -> 'a t
  val difference : 'a t -> 'a t -> 'a t

  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t

  val equals : 'a t -> 'a t -> bool
end
module PrefixTreeBag : Bag = struct
  type 'a t =
    | Empty
    | Node of 'a * int * 'a t * 'a t  (* key, count, left, right *)

  let empty = Empty

let  is_empty = function
    | Empty -> true
    | _ -> false

  let rec add x = function
    | Empty -> Node (x, 1, Empty, Empty)
    | Node (key, count, left, right) when x < key -> Node (key, count, add x left, right)
    | Node (key, count, left, right) when x > key -> Node (key, count, left, add x right)
    | Node (key, count, left, right) -> Node (key, count + 1, left, right)

  let rec remove x = function
    | Empty -> Empty
    | Node (key, count, left, right) when x < key -> Node (key, count, remove x left, right)
    | Node (key, count, left, right) when x > key -> Node (key, count, left, remove x right)
    | Node (_, count, left, right) ->
      if count > 1 then Node (x, count - 1, left, right)
      else match left, right with
        | Empty, _ -> right
        | _, Empty -> left
        | _ ->
          let rec min_key = function
            | Node (k, _, Empty, _) -> k
            | Node (_, _, l, _) -> min_key l
            | Empty -> failwith "Unexpected"
          in
          let min_right = min_key right in
          Node (min_right, 1, left, remove min_right right)

  let rec contains x = function
    | Empty -> false
    | Node (key, _, left, _) when x < key -> contains x left
    | Node (key, _, _, right) when x > key -> contains x right
    | _ -> true

  let rec filter pred = function
    | Empty -> Empty
    | Node (key, count, left, right) ->
      let new_left = filter pred left in
      let new_right = filter pred right in
      if pred key then Node (key, count, new_left, new_right)
      else
        match new_left, new_right with
        | Empty, _ -> new_right
        | _, Empty -> new_left
        | _ -> Node (key, count, new_left, new_right)

  let rec map f = function
    | Empty -> Empty
    | Node (key, count, left, right) ->
      let new_key = f key in
      let new_left = map f left in
      let new_right = map f right in
      Node (new_key, count, new_left, new_right)

  let rec fold_left f acc = function
    | Empty -> acc
    | Node (key, count, left, right) ->
      let acc' = fold_left f acc left in
      let rec repeat acc n =
        if n <= 0 then acc else repeat (f acc key) (n - 1)
      in
      let acc'' = repeat acc' count in
      fold_left f acc'' right

  let rec fold_right f tree acc =
    match tree with
    | Empty -> acc
    | Node (key, count, left, right) ->
      let acc' = fold_right f right acc in
      let rec repeat acc n =
        if n <= 0 then acc else repeat (f key acc) (n - 1)
      in
      let acc'' = repeat acc' count in
      fold_right f left acc''

 let rec union t1 t2 =
   match t1, t2 with
   | Empty, t | t, Empty -> t
   | Node (key1, count1, left1, right1), Node (key2, count2, left2, right2) ->
     if key1 < key2 then
       Node (key1, count1, left1, union right1 t2)
     else if key1 > key2 then
       Node (key2, count2, left2, union right2 t1)
     else
       (* Если ключи одинаковые, складываем кратности *)
       let merged = Node (key1, count1 + count2, union left1 left2, union right1 right2) in
       merged


  let rec intersection t1 t2 =
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Node (key, count, left, right), _ ->
      let new_left = intersection left t2 in
      let new_right = intersection right t2 in
      if contains key t2 then
        Node (key, count, new_left, new_right)
      else
        match new_left, new_right with
        | Empty, _ -> new_right
        | _, Empty -> new_left
        | _ -> Node (key, count, new_left, new_right)

  let rec difference t1 t2 =
    match t1 with
    | Empty -> Empty
    | Node (key, count, left, right) ->
      let new_left = difference left t2 in
      let new_right = difference right t2 in
      if contains key t2 then
        match new_left, new_right with
        | Empty, _ -> new_right
        | _, Empty -> new_left
        | _ -> Node (key, count, new_left, new_right)
      else
        Node (key, count, new_left, new_right)

  let rec to_list = function
    | Empty -> []
    | Node (key, count, left, right) ->
      let rec repeat acc n = if n <= 0 then acc else repeat (key :: acc) (n - 1) in
      to_list left @ repeat [] count @ to_list right

  let of_list lst =
    let rec build_tree acc = function
      | [] -> acc
      | x :: xs -> build_tree (add x acc) xs
    in
    build_tree empty lst

  let rec equals t1 t2 =
    match t1, t2 with
    | Empty, Empty -> true
    | Node (k1, c1, l1, r1), Node (k2, c2, l2, r2) ->
      k1 = k2 && c1 = c2 && equals l1 l2 && equals r1 r2
    | _ -> false
end
