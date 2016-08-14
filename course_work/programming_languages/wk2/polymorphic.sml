datatype 'a my_option = MY_NONE | MY_SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                       | Leaf of 'b

(* type is (int, int) tree -> int *)
fun sum_tree tr =
  case tr of 
       Leaf i            => i
     | Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

(* type is ('a, int) tree -> int *)
fun sum_leaves tr =
  case tr of 
       Leaf i            => i
     | Node(_, lft, rgt) => sum_leaves lft + sum_leaves rgt

(* type is ('a, 'b) tree -> int *)
fun num_leaves tr =
  case tr of 
       Leaf i            => 1
     | Node(_, lft, rgt) => num_leaves lft + num_leaves rgt

(* ''a * ''a -> string *)
fun same_thing(x, y) =
  if x = y then "yes" else "no"
