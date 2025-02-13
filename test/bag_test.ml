open Bag
open Alcotest
open QCheck

module B = PrefixTreeBag

let test_empty_is_empty () =
  let bag = B.empty in
  check Alcotest.bool "empty bag should be empty" true (B.is_empty bag);
  check Alcotest.bool "empty bag should not contain 1" false (B.contains 1 bag)

let test_add_contains () =
  let bag = B.add 1 B.empty in
  check Alcotest.bool "bag contains 1 after adding" true (B.contains 1 bag);
  check Alcotest.bool "bag does not contain 2" false (B.contains 2 bag)

let test_remove () =
  let bag = B.add 1 B.empty in
  let bag = B.add 2 bag in
  let bag = B.remove 1 bag in
  check Alcotest.bool "bag does not contain 1 after removal" false (B.contains 1 bag);
  check Alcotest.bool "bag contains 2 after removal" true (B.contains 2 bag)

let test_filter () =
  let bag = B.of_list [1; 2; 3; 4; 5] in
  let bag = B.filter (fun x -> x mod 2 = 0) bag in
  check Alcotest.(list int) "filter even numbers" [2; 4] (B.to_list bag)

let test_map () =
  let bag = B.of_list [1; 2; 3; 4; 5] in
  let bag = B.map (fun x -> x * 2) bag in
  check Alcotest.(list int) "map to double values" [2; 4; 6; 8; 10] (B.to_list bag)

let test_fold_left () =
  let bag = B.of_list [1; 2; 3; 4; 5] in
  let sum = B.fold_left (+) 0 bag in
  check Alcotest.int "fold_left sum" 15 sum

let test_fold_right () =
  let bag = B.of_list [1; 2; 3; 4; 5] in
  let concat = B.fold_right (fun x acc -> string_of_int x ^ acc) bag "" in
  check Alcotest.string "fold_right concat" "12345" concat

let test_union () =
  let bag1 = B.of_list [1; 2; 3] in
  let bag2 = B.of_list [3; 4; 5] in
  let bag = B.union bag1 bag2 in
  check Alcotest.(list int) "union of two bags" [1; 2; 3; 3; 4; 5] (B.to_list bag)

let test_intersection () =
  let bag1 = B.of_list [1; 2; 3] in
  let bag2 = B.of_list [3; 4; 5] in
  let bag = B.intersection bag1 bag2 in
  check Alcotest.(list int) "intersection of two bags" [3] (B.to_list bag)

let test_difference () =
  let bag1 = B.of_list [1; 2; 3] in
  let bag2 = B.of_list [3; 4; 5] in
  let bag = B.difference bag1 bag2 in
  check Alcotest.(list int) "difference of two bags" [1; 2] (B.to_list bag)

let test_to_list_of_list () =
  let lst = [1; 2; 3; 2; 1] in
  let bag = B.of_list lst in
  let sorted_lst = List.sort compare lst in
  let sorted_lst2 = List.sort compare (B.to_list bag) in
  check Alcotest.(list int) "to_list and of_list" sorted_lst sorted_lst2

let property_union_with_empty () =
  Test.make ~name:"union with empty is identity"
    (list int)
    (fun lst ->
      let bag = B.of_list lst in
      B.equals (B.union bag B.empty) bag)

let property_to_list_of_list_length () =
  Test.make ~name:"to_list and of_list preserve length"
    (list int)
    (fun lst ->
      let bag = B.of_list lst in
      List.length (B.to_list bag) = List.length lst)

let property_union_with_multiple_empty () =
  Test.make ~name:"union with multiple empty is identity"
    (pair (list int) (list int))
    (fun (lst1, lst2) ->
      let bag1 = B.of_list lst1 in
      let bag2 = B.of_list lst2 in
      let empty_bag = B.empty in
      let left = B.union (B.union bag1 empty_bag) bag2 in
      let right = B.union bag1 (B.union empty_bag bag2) in
      B.equals left right)


let property_union_commutative () =
  Test.make ~name:"union is commutative"
    (pair (list int) (list int))
    (fun (lst1, lst2) ->
      let bag1 = B.of_list lst1 in
      let bag2 = B.of_list lst2 in
      B.equals (B.union bag1 bag2) (B.union bag2 bag1))

let property_union_associative () =
  Test.make ~name:"union is associative"
    (triple (list int) (list int) (list int))
    (fun (lst1, lst2, lst3) ->
      let bag1 = B.of_list lst1 in
      let bag2 = B.of_list lst2 in
      let bag3 = B.of_list lst3 in
      let left = B.union (B.union bag1 bag2) bag3 in
      let right = B.union bag1 (B.union bag2 bag3) in
      B.equals left right)

let () =
  let open Alcotest in
  run "PrefixTreeBag tests" [
    "empty/is_empty", [
      test_case "empty bag should be empty" `Quick test_empty_is_empty;
    ];
    "add/contains", [
      test_case "bag contains element after adding" `Quick test_add_contains;
    ];
    "remove", [
      test_case "bag does not contain element after removal" `Quick test_remove;
    ];
    "filter", [
      test_case "filter only even numbers" `Quick test_filter;
    ];
    "map", [
      test_case "map doubles the values" `Quick test_map;
    ];
    "fold_left", [
      test_case "fold_left sums all values" `Quick test_fold_left;
    ];
    "fold_right", [
      test_case "fold_right concatenates all values" `Quick test_fold_right;
    ];
    "union", [
      test_case "union combines two bags" `Quick test_union;
    ];
    "intersection", [
      test_case "intersection finds common elements" `Quick test_intersection;
    ];
    "difference", [
      test_case "difference finds unique elements" `Quick test_difference;
    ];
    "to_list/of_list", [
      test_case "to_list and of_list preserve elements" `Quick test_to_list_of_list;
    ];
    "property-based", [
          QCheck_alcotest.to_alcotest (property_union_with_empty ());
          QCheck_alcotest.to_alcotest (property_to_list_of_list_length ());
          QCheck_alcotest.to_alcotest (property_union_commutative ());
          QCheck_alcotest.to_alcotest (property_union_associative ());
          QCheck_alcotest.to_alcotest (property_union_with_multiple_empty ());

        ];
  ]