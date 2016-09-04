import Control.Monad (liftM3)

{- Paring URL-encoded query string -}

-- An example association list
sampleAList :: [(String, Maybe String)]
sampleAList = [("name",       Just "Attila \"The Hun\""),
               ("occupation", Just "Khan")]

data MovieReview = MovieReview {
  revTitle  :: String
, revUser   :: String
, revReview :: String
}

-- Using (_:_) to ensure the string/list is not empty
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist = case lookup "title" alist of
  Just (Just title@(_:_)) -> case lookup "user" alist of

    Just (Just user@(_:_)) -> case lookup "review" alist of

      Just (Just review@(_:_)) -> Just (MovieReview title user review)
      _                        -> Nothing -- no review

    _                      -> Nothing     -- no user

  _                       -> Nothing      -- no title

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
  title  <- lookup1 "title"  alist
  user   <- lookup1 "user"   alist
  review <- lookup1 "review" alist
  return $ MovieReview title user review

lookup1 key alist = case lookup key alist of
  Just (Just s@(_:_)) -> Just s
  _                   -> Nothing

liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist =
  liftM3 MovieReview (lookup1 "title"  alist)
                     (lookup1 "user"   alist)
                     (lookup1 "review" alist)

{-
 - Note: 'MovieReview' is a contructor function with signature
 - > :t MovieReview :: String -> String -> String -> MovieReview
 -}
apReview :: [(String, Maybe String)] -> Maybe MovieReview
apReview alist =
  MovieReview `liftM` lookup1 "title"  alist
                 `ap` lookup1 "user"   alist
                 `ap` lookup1 "review" alist

{-
 - Note that ($) is really just a non-monadic version of 'ap' or <*>
 -
 - ap can usually be defined as either:
 - > liftM2 id
 - > liftM2 ($)
 -}
