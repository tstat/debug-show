{-# LANGUAGE TemplateHaskell #-}

module Debug.Show.StringExpressionManipulation
  ( parens
  , braces
  , concatExp
  , typeStringExp
  ) where

import Language.Haskell.TH
import Data.Foldable

parens :: Exp -> Q Exp
parens = encloseExp "(" ")"

braces :: Exp -> Q Exp
braces = encloseExp "{" "}"

encloseExp :: String -> String -> Exp -> Q Exp
encloseExp l r strExp = [| l ++ $(pure strExp) ++ r |]

concatExp :: Foldable f => f Exp -> Q Exp
concatExp = foldlM (\b a -> [| $(pure b) ++ $(pure a) |])
            (LitE $ StringL "")

typeStringExp :: Type -> Exp
typeStringExp = LitE . StringL . typeString

typeString :: Type -> String
typeString typ = "<" ++ show (pprParendType typ) ++ ">"
