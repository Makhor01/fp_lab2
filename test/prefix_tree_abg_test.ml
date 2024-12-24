(* test/prefix_tree_bag_tests.ml *)

open Alcotest
open PrefixTreeBag

(* Unit-тесты *)

let test_empty () =
  let bag = empty in
  check bool "Empty bag is empty" true (is_empty bag);
  check (list int) "Empty bag has no elements" [] (to_list bag)

let test_add () =
  let bag = add 1 empty in
  check bool "Bag is not empty after add" false (is_empty bag);
  check (list int) "Bag contains added element" [1] (to_list bag)

let test_remove () =
  let bag = of_list [1; 2; 3] in
  let bag' = remove 2 bag in
  check (list int) "Element is removed" [1; 3] (to_list bag');
  let bag'' = remove 4 bag in
  check (list int) "Removing non-existent element doesn't change bag" [1; 2; 3] (to_list bag'')

let test_union () =
  let bag1 = of_list [1; 2] in
  let bag2 = of_list [3; 4] in
  let result = union bag1 bag2 in
  check (list int) "Union contains all elements" [1; 2; 3; 4] (to_list result)

let test_intersection () =
  let bag1 = of_list [1; 2; 3] in
  let bag2 = of_list [3; 4; 5] in
  let result = intersection bag1 bag2 in
  check (list int) "Intersection contains common elements" [3] (to_list result)

let test_difference () =
  let bag1 = of_list [1; 2; 3] in
  let bag2 = of_list [3; 4; 5] in
  let result = difference bag1 bag2 in
  check (list int) "Difference contains elements in bag1 not in bag2" [1; 2] (to_list result)

(* Property-based тесты *)

let prop_union_associativity =
  QCheck.Test.make
    ~name:"Union is associative"
    (triple (list int) (list int) (list int))
    (fun (a, b, c) ->
       let bag_a = of_list a in
       let bag_b = of_list b in
       let bag_c = of_list c in
       let left = union bag_a (union bag_b bag_c) in
       let right = union (union bag_a bag_b) bag_c in
       to_list left = to_list right)

let prop_union_neutral_element =
  QCheck.Test.make
    ~name:"Union with empty is identity"
    (list int)
    (fun a ->
       let bag = of_list a in
       to_list (union bag empty) = to_list bag)

let prop_add_remove_invariant =
  QCheck.Test.make
    ~name:"Add and remove preserve identity"
    (pair (list int) int)
    (fun (a, x) ->
       let bag = of_list a in
       let bag' = add x bag |> remove x in
       to_list bag = to_list bag')

(* Запуск всех тестов *)

let () =
  Alcotest.run "PrefixTreeBag Tests" [
    "Unit Tests", [
      "test_empty", `Quick, test_empty;
      "test_add", `Quick, test_add;
      "test_remove", `Quick, test_remove;
      "test_union", `Quick, test_union;
      "test_intersection", `Quick, test_intersection;
      "test_difference", `Quick, test_difference;
    ];
    "Property-Based Tests", QCheck_alcotest.to_alcotest [
      prop_union_associativity;
      prop_union_neutral_element;
      prop_add_remove_invariant;
    ];
  ]
