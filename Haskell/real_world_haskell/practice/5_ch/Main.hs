import SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("basr", JBool False)])
