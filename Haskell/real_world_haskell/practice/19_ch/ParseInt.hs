{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Utils

import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

import Data.Char (isDigit)

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)

newtype Parser a = P {
  runP :: ExceptT ParseError (State B.ByteString) a
} deriving (Functor, Applicative, Monad, MonadError ParseError)

{-
 - The above 'newtype' declaration deliberately avoid deriving 'MonadState
 - B.byteString', to prevent users from using 'get' or 'put' directly. As a
 - result, we will have to manually lift to get at the 'State' monad in the
 - transformer stack.
 -}
liftP :: State B.ByteString a -> Parser a
liftP m = P $ lift m

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing       -> throwError EndOfInput
    Just (c, s')
      | p c       -> liftP (put s') >> return c
      | otherwise -> throwError $ Chatty "satisfy failed"

optional :: Parser a -> Parser (Maybe a)
optional p = (Just `liftM` p) `catchError` (const $ return Nothing)

runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString)
runParser p bs = case (runState . runExceptT . runP $ p) bs of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)

many :: Parser a -> Parser [a]
many p = do
  s <- liftP get
  case B.uncons s of
    Nothing      -> liftP (put s) >> return []
    Just (c, s') -> case (runParser p) (B.pack [c]) of
      Left err      -> (liftP . put $ c `B.cons` s') >> return []
      Right (r, bs) -> (liftP $ put s') >> (r:) <$> many p

intParser :: Parser Int
intParser = (many $ satisfy isDigit) >>= return . read

intParserNeg :: Parser Int
intParserNeg =
  let isMinusSign   = (== '-')
      canBeNegative = (isDigit &&& isMinusSign) >>> uncurry (||)
  in (many $ satisfy canBeNegative) >>= return . read

