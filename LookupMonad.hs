module LookupMonad where

lookupM :: (Show a, Eq a, Monad m) => a -> [(a, b)] -> m b

lookupM name env = case (lookup name env) of
  (Just value) -> return value
  Nothing -> fail ("Could not find " ++ (show name))

