module State where

{-
 - Implicitly passing state with Monads:
 -
 - Generally, when given a state value, we inspect it and produce a result
 - and a new state value. Let state can be any type s and result be any type
 - a, the type signature becomes: 's -> (a, s)'.
 -   ie: take a state 's', do something with it, and return a result 'a' and
 -       possible a new state 's'
 -}

type SimpleState s a = s -> (a, s)
type StringState a   = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

returnAlt' :: a -> SimpleState s a
returnAlt' = (,)

bindSt :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                      in  (k a) s'

{-
 - For better readability/comprehension:
 -   m == step
 -   k == makeStep
 -   s == oldState
 -}
bindAlt :: (s -> (a, s))      -- step
        -> (a -> s -> (b, s)) -- makeStep
        -> (s -> (b, s))      -- (makeStep result) newState
bindAlt step makeStep oldState =
  let (result, newState) = step oldState
  in  (makeStep result) newState

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

-- Note: runState :: State s a -> s -> (a, s)
newtype State s a = State {
  runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
