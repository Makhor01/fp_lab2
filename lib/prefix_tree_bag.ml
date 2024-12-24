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
end

module PrefixTreeBag : Bag = struct
  type 'a t = Node of (int * ('a * 'a t) list)

  let empty = Node (0, [])

  let is_empty = function
    | Node (size, _) -> size = 0

  let rec add x (Node (size, children)) =
    let rec insert = function
      | [] -> [(x, Node (0, []))]
      | (key, subtree) :: rest when key = x -> (key, add x subtree) :: rest
      | child :: rest -> child :: insert rest
    in
    Node (size + 1, insert children)

  let rec remove x (Node (size, children)) =
    let rec delete = function
      | [] -> []
      | (key, subtree) :: rest when key = x -> rest
      | child :: rest -> child :: delete rest
    in
    Node (max 0 (size - 1), delete children)

  let rec contains x (Node (_, children)) =
    List.exists (fun (key, _) -> key = x) children

  let filter pred (Node (size, children)) =
    let filtered_children =
      List.filter (fun (key, _) -> pred key) children
    in
    Node (List.length filtered_children, filtered_children)

  let rec map f (Node (size, children)) =
  let mapped_children =
    List.map (fun (key, subtree) -> (f key, map f subtree)) children
  in
  Node (size, mapped_children)


  let rec fold_left f acc (Node (_, children)) =
    List.fold_left
      (fun acc (key, subtree) -> fold_left f (f acc key) subtree)
      acc children

  let rec fold_right f (Node (_, children)) acc =
    List.fold_right
      (fun (key, subtree) acc -> f key (fold_right f subtree acc))
      children acc

  let rec union (Node (_, children1)) (Node (_, children2)) =
    let combined_children = children1 @ children2 in
    Node (List.length combined_children, combined_children)

  let intersection (Node (_, children1)) (Node (_, children2)) =
    let intersected_children =
      List.filter (fun (key, _) -> List.mem_assoc key children2) children1
    in
    Node (List.length intersected_children, intersected_children)

  let difference (Node (_, children1)) (Node (_, children2)) =
    let diff_children =
      List.filter (fun (key, _) -> not (List.mem_assoc key children2)) children1
    in
    Node (List.length diff_children, diff_children)

  let rec to_list (Node (_, children)) =
    List.flatten (List.map (fun (key, subtree) -> key :: to_list subtree) children)

  let of_list lst =
    List.fold_left (fun acc x -> add x acc) empty lst
end