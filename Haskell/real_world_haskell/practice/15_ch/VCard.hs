import Utils
import Control.Monad

data Context = Home | Mobile | Business
  deriving (Show, Eq)

type Phone = String

albulena :: [(Context, Phone)]
albulena = [(Home, "+355-652-55512")]

nils :: [(Context, Phone)]
nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"),   (Business, "+47-922-25-551")]

twalumba :: [(Context, Phone)]
twalumba = [(Business, "+260-02-55-5121")]

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
  Nothing -> lookup Mobile ps
  Just n  -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter (contextIs Business) ps of
          [] -> filter (contextIs Mobile) ps
          ns -> ns

contextIs :: Context -> (Context, a) -> Bool
contextIs a (b, _) = a == b

contextIs' :: Context -> (Context, a) -> Bool
contextIs' = curry $ ((==) *** fst) >>> app

{-
 - Class definition of 'MonadPlus':
 -
 - class Monad m => MonadPlus m where
 -   mzero :: m a
 -   mplus :: m a -> m a -> m a
 -
 -
 - Instance definitions of 'MonadPlus' for [] and Maybe:
 -
 - instance MonadPlus [] where
 -   mzero = []
 -   mplus = (++)
 -
 - instance MonadPlus Maybe where
 -   mzero = Nothing
 -
 -   Nothing `mplus` ys = ys
 -   xs      `mplus` _  = xs
 -}

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

oneBusinessPhone' :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone' = (lookup Business &&& lookup Mobile) >>> uncurry mplus

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps

allPersonalPhones' :: [(Context, Phone)] -> [Phone]
allPersonalPhones' = (filter (contextIs Home) &&& filter (contextIs Mobile))
  >>> uncurry mplus
  >>> map snd

lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x, y):xys)
  | x == k    = return y `mplus` lookupM k xys
  | otherwise = lookupM k xys

{-
 - Laws of MonadPlus:
 - > mzero >>= f == mzero
 - > v >> mzero  == mzero -- Must shortcircuit and yield mzero
 -}

{-
 - The standard 'guard' function with 'MonadPlus' from 'Control.Monad':
 - guard :: MonadPlus m => Bool -> m ()
 - guard True  = return ()
 - guard False = mzero
 -}

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
