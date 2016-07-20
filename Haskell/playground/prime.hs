-- A supposedly more efficient prime numbers algorithm from stackoverflow
-- @link http://stackoverflow.com/questions/11768958/prime-sieve-in-haskell
isPrime   :: (Integral a) => a -> Bool
isPrime n =  go 2
  where go d
          | d * d > n      = True
          | n `rem` d == 0 = False
          | otherwise      = go (d + 1)

primes :: (Integral a) => [a]
primes =  filter isPrime [2..]
