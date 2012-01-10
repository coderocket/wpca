module ParserMonad where

data P a = Ok a | Failed String

instance Monad P where
  return a = Ok a
  (Ok a) >>= k = k a
  (Failed e) >>= k = Failed e

instance Show a => Show (P a) where
  show (Ok a) = show a
  show (Failed err) = err
 
failP :: String -> P a
failP err = Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k =
   case m of
      Ok a -> Ok a
      Failed e -> k e

