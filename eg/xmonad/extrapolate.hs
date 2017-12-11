{-# LANGUAGE TemplateHaskell #-}
import qualified Test.LeanCheck as Lean
import Test.Extrapolate as Extra

import Data.Map (Map)
import XMonad.StackSet

deriveListableCascading ''StackSet

instance (Generalizable a, Generalizable b) => Generalizable (Map a b) where
  expr = showConstant
  name xys = name (argTy1of2 xys) ++ name (argTy2of2 xys) ++ "s"
  instances xys = this xys id

deriveGeneralizableCascading ''StackSet

main :: IO ()
main = do
  putStrLn "LeanCheck:"
  Lean.check prop_delete
  putStrLn ""

  putStrLn "Extrapolate"
  check prop_delete
  putStrLn ""

type Tag = Int
type Window = Char
type T = StackSet Tag Int Window Int Int

prop_delete :: T -> Bool
prop_delete x =
  case peek x of
  Nothing -> True
  Just i  -> not (member i (delete i x))
