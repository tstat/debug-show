{-# LANGUAGE TemplateHaskell #-}

import Debug.Show (debugShow)
import Structures

main :: IO ()
main = do
  putStrLn $ $(debugShow ''X) x
  putStrLn $ $(debugShow ''Y) $(debugShow ''Loc) $(debugShow ''X) y
  putStrLn $ $(debugShow ''R) r
