{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Debug.Show.TypeManipulation where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable (foldl')
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Language.Haskell.TH

data BoundTypeVar a
  = BTV { _btvType    :: Type
        , _btvFuncVar :: a
        } deriving (Show)

type BoundTypeVars = [BoundTypeVar Exp]

bindTypeVariable :: Type -> Q (BoundTypeVar Name)
bindTypeVariable t = BTV t <$> newName "func"

lookupBTV :: Type -> BoundTypeVars -> Maybe Exp
lookupBTV t = fmap _btvFuncVar . find (\(BTV bt _) -> t == bt)

newtype FieldName
  = FieldName { _unFieldName :: Name
              } deriving (Eq, Ord)

instance Show FieldName where
  show (FieldName fn) = nameBase fn

data FieldInfo
  = FieldInfo { _fiName    :: Maybe FieldName
              , _fiBinding :: Name
              , _fiType    :: Type
              } deriving (Show, Eq, Ord)

----------------------------------------------------------------------
-- When building our expression we may treat an individual field in a
-- number of different ways. There are several checks we may employ in
-- order to determine how we will handle a field. Some of these checks
-- require access to the Q monad. The FieldTreatmentDecider abstraction
-- helps simplify this process by allowing us to break up the
-- different checks into multiple small, focused functions.
----------------------------------------------------------------------

type FieldTreatmentDecider = BoundTypeVars -> FieldInfo -> MaybeT Q FieldTreatment

data FieldTreatment
  = NoInfo
  | TypeConstant TypeConstructor [TypeParam]
  | HasShowInstance
  | TypeVariable Exp
  deriving (Show, Eq, Ord)

newtype TypeConstructor
  = TypeConstructor
  { _unTypeConstructor :: Name
  } deriving (Show, Eq, Ord)

hasShowInstance :: FieldTreatmentDecider
hasShowInstance _ fi = do
  b <- lift $ isInstance ''Show [_fiType fi]
  MaybeT . pure $ if b then Just HasShowInstance else Nothing

isTypeVariable :: FieldTreatmentDecider
isTypeVariable btvs fi = MaybeT . pure $
  case lookupBTV (_fiType fi) btvs of
    Just e  -> Just (TypeVariable e)
    Nothing -> Nothing

isTypeConstant :: FieldTreatmentDecider
isTypeConstant btvs fi = MaybeT . sequenceA $ f <$> getTypeName (_fiType fi)
  where
    f :: TypeConstructor -> Q FieldTreatment
    f tc = do
      typeParams <- either fail pure . getTypeParameters btvs $ _fiType fi
      pure $ TypeConstant tc typeParams

    getTypeName :: Type -> Maybe TypeConstructor
    getTypeName (AppT typ _)    = getTypeName typ
    getTypeName (SigT typ _)    = getTypeName typ
    getTypeName (ConT nam)      = Just $ TypeConstructor nam
    getTypeName (PromotedT nam) = Just $ TypeConstructor nam
    getTypeName _               = Nothing

determineFieldTreatment'
  :: [FieldTreatmentDecider]
  -> BoundTypeVars
  -> FieldInfo
  -> Q FieldTreatment
determineFieldTreatment' xs x y = fromMaybe NoInfo <$> runMaybeT (foldl' f mzero xs)
  where
    f :: MaybeT Q FieldTreatment -> FieldTreatmentDecider -> MaybeT Q FieldTreatment
    f ti tid = mplus ti (tid x y)

determineFieldTreatment :: BoundTypeVars -> FieldInfo -> Q FieldTreatment
determineFieldTreatment = determineFieldTreatment' [ isTypeVariable
                                                   , hasShowInstance
                                                   , isTypeConstant
                                                   ]

----------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------

tyVarToType :: TyVarBndr -> Type
tyVarToType (PlainTV nam)    = VarT nam
tyVarToType (KindedTV nam _) = VarT nam

filterPhantoms :: [Role] -> [TyVarBndr] -> [Type]
filterPhantoms rs = fmap (tyVarToType . snd)
                  . filter (\(a,_) -> a /= PhantomR)
                  . zip rs

data TypeParam
  = Constant FieldInfo
  | Variable Name Exp -- A type variable along with a function that "shows" it
  deriving (Show, Eq, Ord)

typeParamToType :: TypeParam -> Type
typeParamToType (Constant fi)  = _fiType fi
typeParamToType (Variable n _) = VarT n

getTypeParameters :: BoundTypeVars -> Type -> Either String [TypeParam]
getTypeParameters btvs = sequenceA . foldType f []
  where
    f :: [Either String TypeParam] -> Type -> [Either String TypeParam]
    f b (AppT _ t@(ConT nam)) = Right (Constant $ FieldInfo Nothing nam t) : b
    f b (AppT _ (VarT nam)) =
      maybe (Left "debugShow encountered unbound type variable" : b)
      (\e -> Right (Variable nam e) : b)
      (lookupBTV (VarT nam) btvs)
    f b _ = b

foldType :: (b -> Type -> b) -> b -> Type -> b
foldType f b rootTyp =
  case rootTyp of
    a@(ForallT _ _ t)   -> foldType f (f b a) t
    a@(AppT t1 t2)      -> let left = foldType f (f b a) t1
                           in foldType f left t2
    a@(SigT t _)        -> foldType f (f b a) t
    a@(InfixT t1 _ t2)  -> let left = foldType f (f b a) t1
                           in foldType f left t2
    a@(UInfixT t1 _ t2) -> let left = foldType f (f b a) t1
                           in foldType f left t2
    a                   -> f b a

newtype ParentTypes
  = ParentTypes
  { _parentTypes :: Set Type
  } deriving (Show, Eq, Ord, Monoid)
