val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
val w = (!y) + (!z) (* 85 *)
(* x + 1 does not type-check *)

(* Note:
 * - All x, y and z are of type 'int ref'
 *
 * - Variables such as x, y or z are still immutable
 *   - Only the refence contents are mutable
 *)

