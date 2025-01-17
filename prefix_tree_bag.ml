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

  let remove x (Node (size, children)) =
    let rec delete = function
      | [] -> []
      | (key, subtree) :: rest when key = x ->
      let _ = subtree in rest
      | child :: rest -> child :: delete rest
    in
    Node (max 0 (size - 1), delete children)

  let contains x (Node (_, children)) =
    List.exists (fun (key, _) -> key = x) children

  let filter pred (Node (size, children)) =
  let _ = size in
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

  let union (Node (_, children1)) (Node (_, children2)) =
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

(* Тестовый блок с использованием Alcotest *)
let () =
  let module B = PrefixTreeBag in
  let open B in

  (* Тестирование empty и is_empty *)
  let test_empty_is_empty () =
    let bag = empty in
    Alcotest.(check bool) "empty bag should be empty" true (is_empty bag);
    Alcotest.(check bool) "empty bag should not contain 1" false (contains 1 bag)
  in

  (* Тестирование add и contains *)
  let test_add_contains () =
    let bag = add 1 empty in
    Alcotest.(check bool) "bag contains 1 after adding" true (contains 1 bag);
    Alcotest.(check bool) "bag does not contain 2" false (contains 2 bag)
  in

  (* Тестирование remove *)
  let test_remove () =
    let bag = add 1 empty in
    let bag = add 2 bag in
    let bag = remove 1 bag in
    Alcotest.(check bool) "bag does not contain 1 after removal" false (contains 1 bag);
    Alcotest.(check bool) "bag contains 2 after removal" true (contains 2 bag)
  in

  (* Тестирование filter *)
  let test_filter () =
    let bag = of_list [1; 2; 3; 4; 5] in
    let bag = filter (fun x -> x mod 2 = 0) bag in
    Alcotest.(check (list int)) "filter even numbers" [2; 4] (to_list bag)
  in

  (* Тестирование map *)
  let test_map () =
    let bag = of_list [1; 2; 3; 4; 5] in
    let bag = map (fun x -> x * 2) bag in
    Alcotest.(check (list int)) "map to double values" [2; 4; 6; 8; 10] (to_list bag)
  in

  (* Тестирование fold_left *)
  let test_fold_left () =
    let bag = of_list [1; 2; 3; 4; 5] in
    let sum = fold_left (+) 0 bag in
    Alcotest.(check int) "fold_left sum" 15 sum
  in

  (* Тестирование fold_right *)
  let test_fold_right () =
    let bag = of_list [1; 2; 3; 4; 5] in
    let concat = fold_right (fun x acc -> string_of_int x ^ acc) bag "" in
    Alcotest.(check string) "fold_right concat" "12345" concat
  in

  (* Тестирование union *)
  let test_union () =
    let bag1 = of_list [1; 2; 3] in
    let bag2 = of_list [3; 4; 5] in
    let bag = union bag1 bag2 in
    let sorted_list = List.sort compare (to_list bag) in
    Alcotest.(check (list int)) "union of two bags" [1; 2; 3; 3; 4; 5] sorted_list
  in

  (* Тестирование intersection *)
  let test_intersection () =
    let bag1 = of_list [1; 2; 3] in
    let bag2 = of_list [3; 4; 5] in
    let bag = intersection bag1 bag2 in
    Alcotest.(check (list int)) "intersection of two bags" [3] (to_list bag)
  in

  (* Тестирование difference *)
  let test_difference () =
    let bag1 = of_list [1; 2; 3] in
    let bag2 = of_list [3; 4; 5] in
    let bag = difference bag1 bag2 in
    Alcotest.(check (list int)) "difference of two bags" [1; 2] (to_list bag)
  in

  (* Тестирование to_list и of_list *)
  let test_to_list_of_list () =
    let lst = [1; 2; 3; 2; 1] in
    let bag = of_list lst in
    let sorted_lst = List.sort compare lst in
    let sorted_lst2 = List.sort compare (to_list bag) in
    Alcotest.(check (list int)) "to_list and of_list" sorted_lst sorted_lst2
  in

  (* Запуск тестов *)
  (* Запуск тестов *)
  (* Запуск тестов *)
  let tests = [
    "empty/is_empty", [
      Alcotest.test_case "empty bag should be empty" `Quick test_empty_is_empty;
    ];
    "add/contains", [
      Alcotest.test_case "bag contains element after adding" `Quick test_add_contains;
    ];
    "remove", [
      Alcotest.test_case "bag does not contain element after removal" `Quick test_remove;
    ];
    "filter", [
      Alcotest.test_case "filter only even numbers" `Quick test_filter;
    ];
    "map", [
      Alcotest.test_case "map doubles the values" `Quick test_map;
    ];
    "fold_left", [
      Alcotest.test_case "fold_left sums all values" `Quick test_fold_left;
    ];
    "fold_right", [
      Alcotest.test_case "fold_right concatenates all values" `Quick test_fold_right;
    ];
    "union", [
      Alcotest.test_case "union combines two bags" `Quick test_union;
    ];
    "intersection", [
      Alcotest.test_case "intersection finds common elements" `Quick test_intersection;
    ];
    "difference", [
      Alcotest.test_case "difference finds unique elements" `Quick test_difference;
    ];
    "to_list/of_list", [
      Alcotest.test_case "to_list and of_list preserve elements" `Quick test_to_list_of_list;
    ];
  ] in
  Alcotest.run "PrefixTreeBag tests" tests