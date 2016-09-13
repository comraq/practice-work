(* A contrasting example in SML for 'datatypes.rkt' *)

datatype int_or_string = I of int | S of string

fun funny_sum []          = 0
  | funny_sum ((I i)::xs) = i + funny_sum xs
  | funny_sum ((S s)::xs) = String.size s + funny_sum xs

datatype exp = Const    of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp

(* exp -> int *)
fun eval_exp_old (Const i)          = i
  | eval_exp_old (Negate e)         = ~(eval_exp_old e)
  | eval_exp_old (Add(e1, e2))      = eval_exp_old e1 + eval_exp_old e2
  | eval_exp_old (Multiply(e1, e2)) = eval_exp_old e1 * eval_exp_old e2


exception Error of string

(* exp -> exp *)
fun eval_exp_new e =
let fun get_int (Const i) = i
      | get_int _         = raise (Error "Expected Const Constructor!")
in
  case e of
       Const _          => e
     | Negate e'        => Const (~ (get_int (eval_exp_new e') ) )
     | Add(e1, e2)      => Const ( get_int (eval_exp_new e1)
                                + get_int (eval_exp_new e2) )

     | Multiply(e1, e2) => Const ( get_int (eval_exp_new e1)
                                 * get_int (eval_exp_new e2) )
end
