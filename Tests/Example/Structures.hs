module Structures where

data X = X1 Int String (Int -> Int -> String)
       | X2 Int

x :: X
x = X1 3 "hello world" ((show .) . const)

data Y a p b = Y (Z a b)

data Z a b = Z a b

newtype Loc = Loc (Int,Int)

y :: Y Loc () X
y = Y (Z (Loc (3,5)) (X2 11))

data R
  = R { field1 :: Int
      , field2 :: String
      , field3 :: (Int -> Int -> String)
      }

r :: R
r = R 2 "hello world" ((show .) . const)
