module TwoSourceErrors where

expr :: IO String
expr = do
  let x = "foobarbaz" + 4
  return $ 1 ++ "str"