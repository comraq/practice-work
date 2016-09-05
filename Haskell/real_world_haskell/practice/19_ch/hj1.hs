import Control.Exception

catchIt :: ArithException -> Maybe ()
catchIt DivideByZero = Just ()
catchIt _            = Nothing

handler :: () -> IO ()
handler = const $ putStrLn "Caught error: divide by zero"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)
