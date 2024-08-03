open Alcotest
open Aoc.Pvector  (* Replace with the actual name of your module *)

let test_make_vec () =
  let v0 = make_vec 0 0 in
  check int "empty vec length" 0 (len v0);
  check string "empty vec structure" "Root(0, 2, Null)" (tree_to_str v0);

  let v1 = make_vec 1 42 in
  check int "single element vec length" 1 (len v1);
  check string "single element vec structure" "Root(1, 4, Internal(Leaf, Null))" (tree_to_str v1);

  let v2 = make_vec 2 42 in
  check int "two element vec length" 2 (len v2);
  check string "two element vec structure" "Root(2, 4, Internal(Leaf, Null))" (tree_to_str v2);

  let v4 = make_vec 4 10 in
  check int "4 element vec length" 4 (len v4);
  check string "4 element vec structure" "Root(4, 4, Internal(Leaf, Leaf))" (tree_to_str v4);

  let v5 = make_vec 5 10 in
  check int "5 element vec length" 5 (len v5);
  check string "5 element vec structure" "Root(5, 8, Internal(Internal(Leaf, Leaf), Internal(Leaf, Null)))" (tree_to_str v5)

let test_len () =
  check int "empty vec len" 0 (len (make_vec 0 0));
  check int "single element vec len" 1 (len (make_vec 1 42));
  check int "10 element vec len" 10 (len (make_vec 10 7))

let test_order () =
  check int "empty vec order" 2 (order (make_vec 0 0));
  check int "single element vec order" 4 (order (make_vec 1 42));
  check int "10 element vec order" 16 (order (make_vec 10 7))

let test_append () =
  let v0 = make_vec 0 0 in
  let v1 = append 42 v0 in
  check int "append to empty vec" 1 (len v1);
  check int "first element after append" 42 (at 0 v1);

  let v2 = append 43 v1 in
  check int "append to non-empty vec" 2 (len v2);
  check int "first element unchanged" 42 (at 0 v2);
  check int "new element appended" 43 (at 1 v2);

  let v4 = List.fold_left (fun v x -> append x v) v0 [1;2;3;4] in
  check int "multiple appends length" 4 (len v4);
  check (list int) "multiple appends elements" [1;2;3;4] (List.init 4 (fun i -> at i v4))

let test_at () =
  let v = List.fold_left (fun v x -> append x v) (make_vec 0 0) [0;1;2;3;4;5;6;7] in
  List.iteri (fun i x -> check int (Printf.sprintf "element at index %d" i) x (at i v)) [0;1;2;3;4;5;6;7];
  check_raises "out of bounds access (upper)" (Failure "Index out of bounds") (fun () -> ignore (at 8 v));
  check_raises "out of bounds access (lower)" (Failure "Index out of bounds") (fun () -> ignore (at (-1) v))

let test_persistence () =
  let v0 = make_vec 0 0 in
  let v1 = append 1 v0 in
  let v2 = append 2 v1 in
  check int "original vec unchanged" 0 (len v0);
  check int "first append persists" 1 (len v1);
  check int "second append persists" 2 (len v2);
  check int "first element of v1" 1 (at 0 v1);
  check int "first element of v2" 1 (at 0 v2);
  check int "second element of v2" 2 (at 1 v2)

let test_immutability () =
  let v0 = make_vec 3 42 in
  let v1 = append 43 v0 in
  check int "original vec length" 3 (len v0);
  check int "new vec length" 4 (len v1);
  for i = 0 to 2 do
    check int (Printf.sprintf "element %d of v0" i) 42 (at i v0);
    check int (Printf.sprintf "element %d of v1" i) 42 (at i v1)
  done;
  check int "new element in v1" 43 (at 3 v1)

let test_two_element_leaves () =
  let v = make_vec 0 0 |> append 1 |> append 2 |> append 3 |> append 4 in
  check int "length of 4-element vec" 4 (len v);
  check int "first element of first leaf" 1 (at 0 v);
  check int "second element of first leaf" 2 (at 1 v);
  check int "first element of second leaf" 3 (at 2 v);
  check int "second element of second leaf" 4 (at 3 v)

  let test_set () =
    (* Test setting an element in a small vector *)
    let v1 = make_vec 3 42 in
    let v1_updated = set 1 99 v1 in
    check int "original vector unchanged" 42 (at 1 v1);
    check int "updated vector changed" 99 (at 1 v1_updated);
    check int "other elements unchanged" 42 (at 0 v1_updated);
    check int "other elements unchanged" 42 (at 2 v1_updated);
  
    (* Test setting the first element *)
    let v2 = make_vec 5 10 in
    let v2_updated = set 0 100 v2 in
    check int "set first element" 100 (at 0 v2_updated);
    check int "other elements unchanged" 10 (at 1 v2_updated);
  
    (* Test setting the last element *)
    let v3 = make_vec 5 10 in
    let v3_updated = set 4 200 v3 in
    check int "set last element" 200 (at 4 v3_updated);
    check int "other elements unchanged" 10 (at 3 v3_updated);
  
    (* Test setting an element in a larger vector *)
    let v4 = make_vec 20 5 in
    let v4_updated = set 15 300 v4 in
    check int "set element in larger vector" 300 (at 15 v4_updated);
    check int "other elements unchanged" 5 (at 14 v4_updated);
    check int "other elements unchanged" 5 (at 16 v4_updated);
  
    (* Test setting an element multiple times *)
    let v5 = make_vec 3 42 in
    let v5_updated1 = set 1 99 v5 in
    let v5_updated2 = set 1 100 v5_updated1 in
    check int "set element multiple times" 100 (at 1 v5_updated2);
  
    (* Test error cases *)
    check_raises "set out of bounds (upper)" (Failure "Index out of bounds") 
      (fun () -> ignore (set (len v1) 0 v1));
    check_raises "set out of bounds (lower)" (Failure "Index out of bounds")
      (fun () -> ignore (set (-1) 0 v1))

let () =
  run "PersistentVector" [
    "make_vec", [
      test_case "Create vectors of different sizes" `Quick test_make_vec;
    ];
    "len", [
      test_case "Vector lengths" `Quick test_len;
    ];
    "order", [
      test_case "Vector orders" `Quick test_order;
    ];
    "append", [
      test_case "Append elements" `Quick test_append;
    ];
    "at", [
      test_case "Access elements" `Quick test_at;
    ];
    "persistence", [
      test_case "Verify persistence" `Quick test_persistence;
    ];
    "immutability", [
      test_case "Verify immutability" `Quick test_immutability;
    ];
    "two_element_leaves", [
      test_case "Verify two-element leaf behavior" `Quick test_two_element_leaves;
    ];
    "set", [
      test_case "Set elements in vector" `Quick test_set;
    ];
  ]