{-- snippet module --}
module Main where

import PutJSON
import SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])

testJSON :: JValue
testJSON =  (JObject [("foo", JNumber 1), ("bar", JBool False)])
{-- /snippet module --}
