module State where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State m) = State $ \s -> let (a, stateA) = m s
                                   in  (f a, stateA)

instance Applicative (State s) where
  pure x              = State $ \s -> (x, s)
  State f <*> State g = State $ \s -> let (h, state1) = f s
                                          (b, state2) = g state1
                                      in (h b, state2)

instance Monad (State s) where
  return        = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- Note: (State s) represents a state monad, if s is changed then it would
  --       be two DIFFERENT monads
  State h >>= f = State $ \s -> let (a, newState) = h s
                                    (State g)     = f a
                                in  g newState

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count
  deriving (Show)

data Value m = Wrong
             | Num Int
             | Fun (Value m -> m (Value m))

instance Monad m => Show (Value m) where
  show Wrong   =  "<wrong>"
  show (Num i) =  show i
  show (Fun f) =  "<function>"

type Environment m = [(Name, Value m)]

type Counter = Int

tick :: State Counter ()
tick =  getCount >>= putCount . (+1)

getCount :: State Counter Counter
getCount =  State $ \c -> (c, c)

putCount   :: Int -> State Counter ()
putCount c =  State $ \_ -> ((), c)


{- Parsing Functions -}

interp             :: Term -> Environment (State Counter) -> State Counter (Value (State Counter))
interp (Var x) e   =  lookup' x e
interp (Con i) e   =  return $ Num i
interp (Add u v) e =  interp u e >>= (\a ->
                      interp v e >>= (\b ->
                      add a b))
interp (Lam x v) e =  return $ Fun (\a -> interp v $ (x, a):e)
interp (App t u) e =  interp t e >>= (\f ->
                      interp u e >>= (\a ->
                      apply f a))
interp Count e     =  getCount >>= \i -> return (Num i)

lookup'              :: (Monad m) => Name -> Environment m -> m (Value m)
lookup' x []         =  return Wrong
lookup' x ((y, b):e)
  | x == y    = return b
  | otherwise = lookup' x e

add                 :: Value (State Counter) -> Value (State Counter) -> (State Counter) (Value (State Counter))
add (Num i) (Num j) =  tick >> (return $ Num $ i + j)
add a b             =  return Wrong

apply           :: Value (State Counter) -> Value (State Counter) -> (State Counter) (Value (State Counter))
apply (Fun k) a =  tick >> k a
apply f a       =  return Wrong





{- Sample Terms -}

term42 :: Term
term42 =  (App (Lam "x" (Add (Var "x") (Var "x")))
               (Add (Con 10) (Con 11)))

termE :: Term
termE =  (App (Con 1) (Con 2))

termF :: Term
termF =  (Lam "x" (Lam "y" (Lam "z" (Add (Var "x") 
                                         (Add (Var "y") (Var "z"))))))

termS :: Term
termS =  (Add (Add (Con 1) (Con 2)) Count)




{- Test Functions for Different Monads/Contexts -}

testState   :: Term -> String
testState t =
  let (a, s) = runState (interp t []) 0
  in "Value: " ++ show a ++ "; Count: " ++ show s

runMain   :: (Term -> String) -> IO ()
runMain f =  do
  putStrLn $ f term42
  putStrLn $ f termF
  putStrLn $ f termE
  putStrLn $ f termS
