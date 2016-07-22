{-- snippet main --}
-- lines beginning with "--" are comments.

main2 = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"

main1 = interact charsCount
    where charsCount input = show (sum $ map (length . words) (lines input)) ++ "\n"

main = interact charsCount
    where charsCount input = show (sum $ map length (lines input)) ++ "\n"
{-- /snippet main --}
  
