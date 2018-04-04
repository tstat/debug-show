{-# LANGUAGE TemplateHaskell #-}

import Debug.Show (debugShow)
import Structures

main :: IO ()
main = do
  putStrLn $ $(debugShow ''X) x
  putStrLn $ $(debugShow ''Y) $(debugShow ''Loc) (const "hello") $(debugShow ''X) y
  putStrLn $ $(debugShow ''R) r
  putStrLn $ $(debugShow ''Z) ($(debugShow ''Y) show undefined show) show z
  -- putStrLn $ $(debugShow ''A) a
