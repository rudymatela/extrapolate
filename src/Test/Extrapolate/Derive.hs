{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Test.Extrapolate.Derive
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This is a module for deriving 'Generalizable' instances.
--
-- Needs GHC and Template Haskell (tested on GHC 8.0).
--
-- If Extrapolate does not compile under later GHCs, this module is the
-- probable culprit.
module Test.Extrapolate.Derive
  ( deriveGeneralizable
  , deriveGeneralizableIfNeeded
  , deriveGeneralizableCascading
  )
where

import Test.Extrapolate.Core (Generalizable(..), Expr ((:$)))
import Test.Extrapolate.TypeBinding
import Language.Haskell.TH
import Test.LeanCheck.Basic
import Test.LeanCheck.Utils.TypeBinding
import Control.Monad (unless, liftM, liftM2, filterM)
import Data.List (delete,nub,sort)
import Data.Char (toLower)
import Data.Typeable

-- | Derives a 'Generalizable' instance for a given type 'Name'.
--
-- Consider the following @Stack@ datatype:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- Writing
--
-- > deriveGeneralizable ''Stack
--
-- will automatically derive the following 'Generalizable' instance:
--
-- > instance Generalizable a => Generalizable (Stack a) where
-- >   tiers = cons2 Stack \/ cons0 Empty
--
-- Needs the @TemplateHaskell@ extension.
deriveGeneralizable :: Name -> DecsQ
deriveGeneralizable = deriveGeneralizableX True False

-- | Same as 'deriveGeneralizable' but does not warn when instance already exists
--   ('deriveGeneralizable' is preferable).
deriveGeneralizableIfNeeded :: Name -> DecsQ
deriveGeneralizableIfNeeded = deriveGeneralizableX False False

-- | Derives a 'Generalizable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
deriveGeneralizableCascading :: Name -> DecsQ
deriveGeneralizableCascading = deriveGeneralizableX True True

deriveGeneralizableX :: Bool -> Bool -> Name -> DecsQ
deriveGeneralizableX warnExisting cascade t = do
  is <- t `isInstanceOf` ''Generalizable
  if is
    then do
      unless (not warnExisting)
        (reportWarning $ "Instance Generalizable " ++ show t
                      ++ " already exists, skipping derivation")
      return []
    else if cascade
           then reallyDeriveGeneralizableCascading t
           else reallyDeriveGeneralizable t

reallyDeriveGeneralizable :: Name -> DecsQ
reallyDeriveGeneralizable t = do
  isEq <- t `isInstanceOf` ''Eq
  isOrd <- t `isInstanceOf` ''Ord
  (nt,vs) <- normalizeType t
  cxt <- sequence [ [t| $(conT c) $(return v) |]
                  | c <- ''Generalizable:([''Eq | isEq] ++ [''Ord | isOrd])
                  , v <- vs]
  cs <- typeConstructorsArgNames t
  let generalizableExpr = mergeIFns $ foldr1 mergeI
        [ do argTypesN <- lookupValN $ "argTypes" ++ show (length ns)
             let exprs = [[| expr $(varE n) |] | n <- ns]
             let conex = foldl AppE (VarE argTypesN) $ (ConE c:map VarE ns)
             let root = [| constant $(stringE $ showJustName c) $(return conex) |]
             let rhs = foldl (\e1 e2 -> [| $e1 :$ $e2 |]) root exprs
             [d| instance Generalizable $(return nt) where
                   expr ($(conP c (map varP ns))) = $rhs |]
        | (c,ns) <- cs
        ]
  let generalizableInstances = do
        n <- newName "x"
        let rhs = foldr1 (\e1 e2 -> [| $e1 . $e2 |])
                         [letin n c ns | (c,ns) <- cs, not (null ns)]
        case (isEq, isOrd) of
          (True, True) ->
               [d| instance Generalizable $(return nt) where
                     instances $(varP n) = these $(stringE vname) $(varE n)
                                         [ constant "==" ((==) -:> $(varE n))
                                         , constant "/=" ((/=) -:> $(varE n))
                                         , constant "<"  ((<)  -:> $(varE n))
                                         , constant "<=" ((<=) -:> $(varE n)) ]
                                         $ $rhs |]
          (True, False) ->
               [d| instance Generalizable $(return nt) where
                     instances $(varP n) = these $(stringE vname) $(varE n)
                                         [ constant "==" ((==) -:> $(varE n))
                                         , constant "/=" ((/=) -:> $(varE n)) ]
                                         $ $rhs |]
          (False, False) ->
               [d| instance Generalizable $(return nt) where
                     instances $(varP n) = this $(stringE vname) $(varE n) $ $rhs |]
          _ -> error $ "reallyDeriveGeneralizable " ++ show t ++ ": the impossible happened"
  cxt |=>| (generalizableExpr `mergeI` generalizableInstances)
  where
  showJustName = reverse . takeWhile (/= '.') . reverse . show
  vname = map toLower . take 1 $ showJustName t

letin :: Name -> Name -> [Name] -> ExpQ
letin x c ns = do
  und <- VarE <$> lookupValN "undefined"
  let lhs = conP c (map varP ns)
  let rhs = return $ foldl AppE (ConE c) [und | _ <- ns]
  let bot = foldl1 (\e1 e2 -> [| $e1 . $e2 |])
                   [ [| instances $(varE n) |] | n <- ns ]
  [| let $lhs = $rhs `asTypeOf` $(varE x) in $bot |]

typeConstructorsArgNames :: Name -> Q [(Name,[Name])]
typeConstructorsArgNames t = do
  cs <- typeConstructors t
  sequence [ do ns <- sequence [newName "x" | _ <- ts]
                return (c,ns)
           | (c,ts) <- cs ]

lookupValN :: String -> Q Name
lookupValN s = do
  mn <- lookupValueName s
  case mn of
    Just n -> return n
    Nothing -> fail $ "lookupValN: cannot find " ++ s


data Bla = Bla Int Int
         | Ble Char
  deriving (Eq, Ord, Show)

-- Not only really derive Generalizable instances,
-- but cascade through argument types.
reallyDeriveGeneralizableCascading :: Name -> DecsQ
reallyDeriveGeneralizableCascading t =
      return . concat
  =<< mapM reallyDeriveGeneralizable
  =<< filterM (liftM not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` ''Generalizable)

-- * Template haskell utilities

typeConArgs :: Name -> Q [Name]
typeConArgs t = do
  is <- isTypeSynonym t
  if is
    then liftM typeConTs $ typeSynonymType t
    else liftM (nubMerges . map typeConTs . concat . map snd) $ typeConstructors t
  where
  typeConTs :: Type -> [Name]
  typeConTs (AppT t1 t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (SigT t _) = typeConTs t
  typeConTs (VarT _) = []
  typeConTs (ConT n) = [n]
#if __GLASGOW_HASKELL__ >= 800
  -- typeConTs (PromotedT n) = [n] ?
  typeConTs (InfixT  t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (UInfixT t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (ParensT t) = typeConTs t
#endif
  typeConTs _ = []

typeConArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
typeConArgsThat t p = do
  targs <- typeConArgs t
  tbs   <- mapM (\t' -> do is <- p t'; return (t',is)) targs
  return [t' | (t',p) <- tbs, p]

typeConCascadingArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConCascadingArgsThat` p = do
  ts <- t `typeConArgsThat` p
  let p' t' = do is <- p t'; return $ t' `notElem` (t:ts) && is
  tss <- mapM (`typeConCascadingArgsThat` p') ts
  return $ nubMerges (ts:tss)

-- Normalizes a type by applying it to necessary type variables, making it
-- accept "zero" parameters.  The normalized type is tupled with a list of
-- necessary type variables.
--
-- Suppose:
--
-- > data DT a b c ... = ...
--
-- Then, in pseudo-TH:
--
-- > normalizeType [t|DT|] == Q (DT a b c ..., [a, b, c, ...])
normalizeType :: Name -> Q (Type, [Type])
normalizeType t = do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT (ConT t) vs, vs)
  where
    newNames :: [String] -> Q [Name]
    newNames = mapM newName
    newVarTs :: Int -> Q [Type]
    newVarTs n = liftM (map VarT)
               $ newNames (take n . map (:[]) $ cycle ['a'..'z'])

-- Normalizes a type by applying it to units (`()`) while possible.
--
-- > normalizeTypeUnits ''Int    === [t| Int |]
-- > normalizeTypeUnits ''Maybe  === [t| Maybe () |]
-- > normalizeTypeUnits ''Either === [t| Either () () |]
normalizeTypeUnits :: Name -> Q Type
normalizeTypeUnits t = do
  ar <- typeArity t
  return (foldl AppT (ConT t) (replicate ar (TupleT 0)))

-- Given a type name and a class name,
-- returns whether the type is an instance of that class.
isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf tn cl = do
  ty <- normalizeTypeUnits tn
  isInstance cl [ty]

isntInstanceOf :: Name -> Name -> Q Bool
isntInstanceOf tn cl = liftM not (isInstanceOf tn cl)

-- | Given a type name, return the number of arguments taken by that type.
-- Examples in partially broken TH:
--
-- > arity ''Int        === Q 0
-- > arity ''Int->Int   === Q 0
-- > arity ''Maybe      === Q 1
-- > arity ''Either     === Q 2
-- > arity ''Int->      === Q 1
--
-- This works for Data's and Newtype's and it is useful when generating
-- typeclass instances.
typeArity :: Name -> Q Int
typeArity t = do
  ti <- reify t
  return . length $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
#else
    TyConI (DataD    _ _ ks _ _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _ _) -> ks
#endif
    TyConI (TySynD _ ks _) -> ks
    _ -> error $ "error (typeArity): symbol " ++ show t
              ++ " is not a newtype, data or type synonym"

-- Given a type name, returns a list of its type constructor names paired with
-- the type arguments they take.
--
-- > typeConstructors ''()    === Q [('(),[])]
--
-- > typeConstructors ''(,)   === Q [('(,),[VarT a, VarT b])]
--
-- > typeConstructors ''[]    === Q [('[],[]),('(:),[VarT a,AppT ListT (VarT a)])]
--
-- > data Pair a = P a a
-- > typeConstructors ''Pair  === Q [('P,[VarT a, VarT a])]
--
-- > data Point = Pt Int Int
-- > typeConstructors ''Point === Q [('Pt,[ConT Int, ConT Int])]
typeConstructors :: Name -> Q [(Name,[Type])]
typeConstructors t = do
  ti <- reify t
  return . map simplify $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ c  _) -> [c]
#else
    TyConI (DataD    _ _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ _ c  _) -> [c]
#endif
    _ -> error $ "error (typeConstructors): symbol " ++ show t
              ++ " is neither newtype nor data"
  where
  simplify (NormalC n ts)  = (n,map snd ts)
  simplify (RecC    n ts)  = (n,map trd ts)
  simplify (InfixC  t1 n t2) = (n,[snd t1,snd t2])
  trd (x,y,z) = z

isTypeSynonym :: Name -> Q Bool
isTypeSynonym t = do
  ti <- reify t
  return $ case ti of
    TyConI (TySynD _ _ _) -> True
    _                     -> False

typeSynonymType :: Name -> Q Type
typeSynonymType t = do
  ti <- reify t
  return $ case ti of
    TyConI (TySynD _ _ t') -> t'
    _ -> error $ "error (typeSynonymType): symbol " ++ show t
              ++ " is not a type synonym"

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f = g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds = do ds <- qds
                return $ map (`ac` c) ds
#if __GLASGOW_HASKELL__ < 800
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
#else
  where ac (InstanceD o c ts ds) c' = InstanceD o (c++c') ts ds
        ac d                     _  = d
#endif

mergeIFns :: DecsQ -> DecsQ
mergeIFns qds = do ds <- qds
                   return $ map m' ds
  where
  m' (InstanceD o c ts ds) = InstanceD o c ts [foldr1 m ds]
  FunD n cs1 `m` FunD _ cs2 = FunD n (cs1 ++ cs2)

mergeI :: DecsQ -> DecsQ -> DecsQ
qds1 `mergeI` qds2 = do ds1 <- qds1
                        ds2 <- qds2
                        return $ ds1 `m` ds2
  where
  [InstanceD o c ts ds1] `m` [InstanceD _ _ _ ds2] = [InstanceD o c ts (ds1 ++ ds2)]

whereI :: DecsQ -> [Dec] -> DecsQ
qds `whereI` w = do ds <- qds
                    return $ map (`aw` w) ds
  where aw (InstanceD o c ts ds) w' = InstanceD o c ts (ds++w')
        aw d                     _  = d

-- > nubMerge xs ys == nub (merge xs ys)
-- > nubMerge xs ys == nub (sort (xs ++ ys))
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge [] ys = ys
nubMerge xs [] = xs
nubMerge (x:xs) (y:ys) | x < y     = x :    xs  `nubMerge` (y:ys)
                       | x > y     = y : (x:xs) `nubMerge`    ys
                       | otherwise = x :    xs  `nubMerge`    ys

nubMerges :: Ord a => [[a]] -> [a]
nubMerges = foldr nubMerge []
