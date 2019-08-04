{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Test.Extrapolate.Generalizable.Derive
-- Copyright   : (c) 2017-2019 Rudy Matela
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
module Test.Extrapolate.Generalizable.Derive
  ( deriveGeneralizable
  , deriveGeneralizableIfNeeded
  , deriveGeneralizableCascading
  )
where

import Test.Extrapolate.Generalizable hiding (Name, isInstanceOf)
import Test.Extrapolate.Utils (foldr0)
import Test.LeanCheck.Derive (deriveListableIfNeeded, deriveListableCascading)
import Test.LeanCheck.Utils.TypeBinding ((-:>))

import Language.Haskell.TH
import Data.Haexpress.Utils.TH

import Control.Monad (liftM, filterM)
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Data.List (delete)


-- | Derives a 'Generalizable' instance for a given type 'Name'.
--
-- If needed, this function also automatically derivates
-- 'Listable', 'Express' and 'Name' instances using respectively
-- 'deriveListable', 'deriveExpress' and 'deriveName'.
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
-- >   instances s = this "s" s
-- >               $ let Stack x y = Stack undefined undefined `asTypeOf` s
-- >                 in instances x
-- >                  . instances y
--
-- This function needs the @TemplateHaskell@ extension.
deriveGeneralizable :: Name -> DecsQ
deriveGeneralizable  =  deriveWhenNeededOrWarn ''Express reallyDerive
  where
  reallyDerive  =  reallyDeriveGeneralizableWithRequisites

-- | Same as 'deriveGeneralizable' but does not warn when instance already exists
--   ('deriveGeneralizable' is preferable).
deriveGeneralizableIfNeeded :: Name -> DecsQ
deriveGeneralizableIfNeeded  =  deriveWhenNeeded ''Express reallyDerive
  where
  reallyDerive  =  reallyDeriveGeneralizableWithRequisites

-- | Derives a 'Generalizable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
deriveGeneralizableCascading :: Name -> DecsQ
deriveGeneralizableCascading = deriveWhenNeeded ''Express reallyDerive
  where
  reallyDerive t  =  concat
                 <$> sequence [ deriveListableCascading t
                              , deriveNameCascading t
                              , deriveExpressCascading t
                              , reallyDeriveGeneralizableCascading t ]

reallyDeriveGeneralizableWithRequisites :: Name -> DecsQ
reallyDeriveGeneralizableWithRequisites t  =  concat <$>
  sequence [ deriveListableIfNeeded t
           , deriveNameIfNeeded t
           , deriveExpressIfNeeded t
           , reallyDeriveGeneralizable t ]

reallyDeriveGeneralizable :: Name -> DecsQ
reallyDeriveGeneralizable t = do
  isEq <- t `isInstanceOf` ''Eq
  isOrd <- t `isInstanceOf` ''Ord
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [ [t| $(conT c) $(return v) |]
#else
  -- template-haskell <= 2.9.0.0:
  cxt <- sequence [ classP c [return v]
#endif
                  | c <- ''Generalizable:([''Eq | isEq] ++ [''Ord | isOrd])
                  , v <- vs]
  cs <- typeConstructorsArgNames t
  asName <- newName "x"
  let generalizableBackground = do
        n <- newName "x"
        case (isEq, isOrd) of
          (True, True) ->
            [d| instance Generalizable $(return nt) where
                  background $(varP n) = [ value "==" ((==) -:> $(varE n))
                                         , value "/=" ((/=) -:> $(varE n))
                                         , value "<"  ((<)  -:> $(varE n))
                                         , value "<=" ((<=) -:> $(varE n)) ] |]
          (True, False) ->
            [d| instance Generalizable $(return nt) where
                  background $(varP n) = [ value "==" ((==) -:> $(varE n))
                                         , value "/=" ((/=) -:> $(varE n)) ] |]
          (False, False) ->
            [d| instance Generalizable $(return nt) where
                  background $(varP n) = [] |]
          _ -> error $ "reallyDeriveGeneralizable " ++ show t ++ ": the impossible happened"
  let generalizableInstances = do
        n <- newName "x"
        let lets = [letin n c ns | (c,ns) <- cs, not (null ns)]
        let rhs = foldr0 (\e1 e2 -> [| $e1 . $e2 |]) [|id|] lets
        [d| instance Generalizable $(return nt) where
              subInstances $(varP n) = $rhs |]
  cxt |=>| (generalizableBackground `mergeI` generalizableInstances)

-- Not only really derive Generalizable instances,
-- but cascade through argument types.
reallyDeriveGeneralizableCascading :: Name -> DecsQ
reallyDeriveGeneralizableCascading t =
      return . concat
  =<< mapM reallyDeriveGeneralizable
  =<< filterM (liftM not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` ''Generalizable)

letin :: Name -> Name -> [Name] -> ExpQ
letin x c ns = do
  und <- VarE <$> lookupValN "undefined"
  let lhs = conP c (map varP ns)
  let rhs = return $ foldl AppE (ConE c) [und | _ <- ns]
  let bot = foldl1 (\e1 e2 -> [| $e1 . $e2 |])
                   [ [| instances $(varE n) |] | n <- ns ]
  [| let $lhs = $rhs `asTypeOf` $(varE x) in $bot |]
