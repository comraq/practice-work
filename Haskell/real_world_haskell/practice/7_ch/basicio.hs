main :: IO ()
main = do
  putStrLn "Greetings! What is your name?"
  name <- getLine
  putStrLn $ "Welcome to Haskell, " ++ name ++ "!"

{-
 - IO Actions:
 - - Have the type IO tl
 - - Are first-class values in Haskell and fit seamlessly with Haskell's
 -   type system.
 - - Produce an effect when performed, but not when evaluated. That is, they
 -   produce an effect only when called by something else in an I/O context.
 - - Any expression may produce an action as its value, but the action will
 -   not perform I/O until it is executed inside another I/O action (or it is
 -   the 'main' function)
 - - Performing (executing) an action of type IO t may perform I/O and will
 -   ultimately deliver a result of type t
 -}

name2reply :: String -> String
name2reply name =
  let charcount = show . length
  in "Pleased to meet you, " ++ name ++ ".\n" ++
     "Your name contains " ++ charcount name ++ " characters."

main2 :: IO ()
main2 = do
  putStrLn "Greetings once again. What is your name?"
  name <- getLine
  let reply = name2reply name
  putStrLn reply

main3 :: IO ()
main3 =
  putStrLn "Greetings! What is your name?" >>
  getLine >>=
  putStrLn . (++) "Welcome to Haskell, " . (++ "!")
