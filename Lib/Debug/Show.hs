{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Debug.Show (debugShow) where

import           Control.Monad
import           Data.Foldable (foldlM, foldl')
import           Data.List (intersperse)
import           Data.Set (Set)
import qualified Data.Set as S
import           Debug.Show.StringExpressionManipulation
import           Debug.Show.TypeManipulation
import           Language.Haskell.TH

debugShow :: Name -> Q Exp
debugShow ty = debugShow' mempty ty

debugShow' :: ParentTypes -> Name -> Q Exp
debugShow' (ParentTypes pts) ty = do
  (ts,cs) <- reifyAndExtract ty
  let currentType = foldl' AppT (ConT ty) ts
  if S.member currentType pts
    then fail "debugShow: Recursive unshowable types detected"
    else pure ()
  buildExp (ParentTypes $ S.insert currentType pts) cs ts

reifyAndExtract :: Name -> Q ([Type],[Con])
reifyAndExtract x = do
    ty <- reify x
    case ty of
      TyConI dec -> pure $ (\(x,y) -> (tyVarToType <$> x, y)) $ extractCon dec
      _          -> fail "debugShow must be used on a plain type constructor"

extractCon :: Dec -> ([TyVarBndr],[Con])
extractCon dec =
  case dec of
    (DataD _ _ ts _ cs _)     -> (ts, cs)
    (NewtypeD _ _ ts _ con _) -> (ts,[con])
    _                         ->
      error $ "debugShow is only defined for data and newtype constructors"

-- | Given a list of constructors and a list of type parameters, build
-- a lambda expression that will return an informative string of value
-- and type information about the structure.
buildExp :: ParentTypes -> [Con] -> [Type] -> Q Exp
buildExp pts cs tyVars = do
  cVar <- newName "c"
  btvs <- traverse bindTypeVariable tyVars
  let patVars = (VarP . _btvFuncVar <$> btvs) ++ [VarP cVar]
  let btves = (\(BTV t fv) -> BTV t (VarE fv)) <$> btvs
  LamE patVars . CaseE (VarE cVar) <$> traverse (conToMatch pts btves) cs

conToMatch :: ParentTypes -> BoundTypeVars -> Con -> Q Match
conToMatch pts btvs con = do
  fis <- traverse (uncurry mkFieldInfo) partialFieldInfo
  let varPs = VarP . _fiBinding <$> fis
  e <- genAndFormatExp fis
  pure $ Match (ConP constructorName varPs) (NormalB e) []
  where
    mkFieldInfo :: Maybe FieldName -> Type -> Q FieldInfo
    mkFieldInfo mfn typ = (\b -> FieldInfo mfn b typ) <$> newName "var"

    constructorName :: Name
    constructorName = case con of
      NormalC nam _ -> nam
      RecC nam _    -> nam
      _             -> error "debugShow only accepts normal constructors"

    partialFieldInfo :: [(Maybe FieldName, Type)]
    partialFieldInfo = case con of
      NormalC _ xs -> (Nothing,) . snd <$> xs
      RecC _ xs    -> (\(fn,_,x) -> (Just (FieldName fn), x)) <$> xs
      _            -> error "debugShow only accepts normal constructors"

    fieldFormatter :: FieldInfo -> Exp -> Q Exp
    fieldFormatter fi = case con of
      RecC _ _ -> maybe pure (\fn e -> [| fn ++ " = " ++ $(pure e) |])
                  (show <$> _fiName fi)
      _        -> parens

    finalFormatter :: [Exp] -> Q Exp
    finalFormatter fs = do
      let cname = show constructorName
      sep <- case con of
               RecC _ _ -> [| ", " |]
               _        -> [| " " |]
      let fieldWrap = case con of
            RecC _ _ -> braces
            _        -> pure
      [| cname ++ " " ++ $(fieldWrap <=< concatExp $ intersperse sep fs) |]

    genAndFormatExp :: [FieldInfo] -> Q Exp
    genAndFormatExp =
      finalFormatter <=<
      traverse
        (\a -> determineFieldTreatment btvs a
           >>= generateExp pts a
           >>= fieldFormatter a)


-- | Generate the rendering expression for a single field.
generateExp :: ParentTypes -> FieldInfo -> FieldTreatment -> Q Exp
generateExp _ (FieldInfo _ _ typ) NoInfo = pure $ typeStringExp typ
generateExp _ (FieldInfo _ bin typ) HasShowInstance =
  [| show $(pure $ VarE bin)
     ++ " :: "
     ++ $(pure $ typeStringExp typ)
   |]
generateExp pts (FieldInfo _ bin _) (TypeConstant typeCons typeParams) = do
  funcExp <- debugShow' pts $ _unTypeConstructor typeCons
  flip AppE (VarE bin) <$> foldlM (applyTypeParam pts) funcExp typeParams
generateExp _ (FieldInfo _ bin _) (TypeVariable tyVarFunc) =
  pure $ AppE tyVarFunc (VarE bin)

applyTypeParam :: ParentTypes -> Exp -> TypeParam -> Q Exp
applyTypeParam pts e1 (Constant fi) = do
  typeInfo <- determineFieldTreatment' [ hasShowInstance ] [] fi
  case typeInfo of
    HasShowInstance -> AppE e1 <$> [| show |]
    _               -> AppE e1 <$> debugShow' pts (_fiBinding fi)
applyTypeParam _ e1 (Variable _ e2) = pure $ AppE e1 e2
