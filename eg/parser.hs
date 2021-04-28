-- Example taken from Lee Pike's SmartCheck:
-- https://github.com/leepike/SmartCheck/blob/master/paper/paper.pdf
-- https://github.com/leepike/smartcheck
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Toy "parser"/"serializer" (with a bug) in And parsing.

module Main where

import Prelude hiding (showList, mod)

--import Test.LeanCheck
import Test.Extrapolate
import qualified Test.LeanCheck as Lean
import Test.LeanCheck.Utils ((-:>))

import Data.List

import GHC.Generics
import Data.Typeable
import Control.Applicative

import Control.Monad.Trans.State
import Data.Char

-----------------------------------------------------------------

-- Let's make up a toy language.

data Lang = Lang
  { modules :: [Mod]
  , funcs   :: [Func]
  } deriving (Show, Read, Typeable, Generic, Eq, Ord)

newtype Var = Var String
  deriving (Show, Read, Typeable, Generic, Eq, Ord)

data Mod = Mod
  { imports :: [Var]
  , exports :: [Var]
  } deriving (Show, Read, Typeable, Generic, Eq, Ord)

data Func = Func
  { fnName :: Var
  , args   :: [Exp]
  , stmts  :: [Stmt]
  } deriving (Show, Read, Typeable, Generic, Eq, Ord)

data Stmt = Assign Var Exp
          | Alloc Var Exp
          | Return Exp
          -- | Ref Exp
          -- | Deref Exp
          -- | Assert Exp
          -- | Loop Exp [Stmt]
          -- | IfTE Exp [Stmt] [Stmt]
  deriving (Show, Read, Typeable, Generic, Eq, Ord)

data Exp = Int Int
         | Bool Bool
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Not Exp
         | And Exp Exp
         | Or Exp Exp
  deriving (Show, Read, Typeable, Generic, Eq, Ord)

instance Listable Var where
  tiers = cons1 Var `suchThat` (\(Var s) -> all isAlphaNum s && not (null s))

deriveListable ''Exp
deriveListable ''Stmt
deriveListable ''Func
deriveListable ''Mod
deriveListable ''Lang

--------------------------------------------------------------------------------

{-
instance Listable Lang where
  tiers = undefined

instance Listable Mod where
  tiers = undefined

instance Listable Func where
  tiers = undefined

instance Listable Stmt where
  tiers = undefined

instance Listable Exp where
  tiers = undefined
-}

{- Alternative:
deriveGeneralizable ''Var
deriveGeneralizable ''Lang
deriveGeneralizable ''Mod
deriveGeneralizable ''Func
deriveGeneralizable ''Exp
deriveGeneralizable ''Stmt
-}
-- {-
instance Name Var  where  name _  =  "v"
instance Name Lang where  name _  =  "l"
instance Name Mod  where  name _  =  "m"
instance Name Func where  name _  =  "f"
instance Name Exp  where  name _  =  "e"
instance Name Stmt where  name _  =  "stmt"

instance Express Var where
  expr (Var v)  =  value "Var" Var :$ expr v

instance Express Lang where
  expr (Lang ms fs)  =  value "Lang" Lang :$ expr ms :$ expr fs

instance Express Mod where
  expr (Mod is es)  =  value "Mod" Mod :$ expr is :$ expr es

instance Express Func where
  expr (Func n as ss)  =  value "Func" Func :$ expr n :$ expr as :$ expr ss

instance Express Exp where
  expr (Int i)      =  value "Int" Int :$ expr i
  expr (Bool p)     =  value "Bool" Bool :$ expr p
  expr (Add e1 e2)  =  value "Add" Add :$ expr e1 :$ expr e2
  expr (Sub e1 e2)  =  value "Sub" Sub :$ expr e1 :$ expr e2
  expr (Mul e1 e2)  =  value "Mul" Mul :$ expr e1 :$ expr e2
  expr (Div e1 e2)  =  value "Div" Div :$ expr e1 :$ expr e2
  expr (Not e)      =  value "Not" Not :$ expr e
  expr (And e1 e2)  =  value "And" And :$ expr e1 :$ expr e2
  expr (Or  e1 e2)  =  value "Or " Or  :$ expr e1 :$ expr e2

instance Express Stmt where
  expr (Assign v e)  =  value "Assign" Assign :$ expr v :$ expr e
  expr (Alloc v e)   =  value "Alloc"  Alloc  :$ expr v :$ expr e
  expr (Return e)    =  value "Return" Return :$ expr e

instance Generalizable Var where
  subInstances v  =  instances (undefined :: [String])

instance Generalizable Lang where
  subInstances l  =  instances (undefined :: [Mod])
                  .  instances (undefined :: [Func])

instance Generalizable Mod where
  subInstances m  =  instances (undefined :: Var)

instance Generalizable Func where
  subInstances f  =  instances (undefined :: Var)
                  .  instances (undefined :: [Exp])
                  .  instances (undefined :: [Stmt])

instance Generalizable Exp where
  background e = [ value "==" ((==) -:> e)
                 , value "/=" ((/=) -:> e) ]
  subInstances  e  =  instances (undefined :: Var)
                   .  instances (undefined :: Int)
                   .  instances (undefined :: Bool)

instance Generalizable Stmt where
  subInstances s  =  instances (undefined :: Var)
                  .  instances (undefined :: Exp)
-- -}


--------------------------------------------------------------------------------
-- "serializer"

parens :: String -> String
parens a = '(' : a ++ ")"

showList :: Show' a => Char -> [a] -> String
showList sep ls = parens $ concat $ intersperse [sep] $ map show' ls

class Show a => Show' a where
  show' :: a -> String
  show' = show

instance Show' Char
instance Show' Int
instance Show' Bool

instance Show' Lang where
  show' (Lang m f)   = unwords
    [ "Lang"
    , showList ';' m
    , showList ';' f
    ]

instance Show' Mod where
  show' (Mod i e)    = unwords
    [ "Mod"
    , showList ':' i
    , showList ':' e
    ]

instance Show' Func where
  show' (Func f a s) = unwords
   [ "Func"
   , show' f
   , showList ',' a
   , showList ',' s
   ]

instance Show' Var where
  show' (Var v) = v

instance Show' Stmt where
  show' stmt         = unwords $ case stmt of
    Assign v e -> ["Assign", show' v, parens $ show' e]
    Alloc v e  -> ["Alloc" , show' v, parens $ show' e]
    Return e   -> ["Return",          parens $ show' e]

instance Show' Exp where
  show' e = unwords $ case e of
    Int i     -> ["Int" , show' i]
    Bool b    -> ["Bool", show' b]
    Add e0 e1 -> ["Add" , parens $ show' e0, parens $ show' e1]
    Sub e0 e1 -> ["Sub" , parens $ show' e0, parens $ show' e1]
    Mul e0 e1 -> ["Mul" , parens $ show' e0, parens $ show' e1]
    Div e0 e1 -> ["Div" , parens $ show' e0, parens $ show' e1]
    Not e0    -> ["Not" , parens $ show' e0]
    And e0 e1 -> ["And" , parens $ show' e0, parens $ show' e1]
    Or  e0 e1 -> ["Or" , parens $ show' e0, parens $ show' e1]

--------------------------------------------------------------------------------
-- "parser"

class Read a => Read' a where
  read' :: String -> a
  read' = read

instance Read' Lang where
  read' str   = run str $ do
    modify (strip "Lang")
    m <- state unparens
    let ms = map read' (fromSeps ';' m)
    f <- state unparens
    let fs = map read' (fromSeps ';' f)
    return (Lang ms fs)

instance Read' Mod where
  read' mod    = run mod $ do
                   modify (strip "Mod")
                   m <- state unparens
                   let i = fromSeps ':' m
                   es <- state unparens
                   let e = fromSeps ':' es
                   return (Mod (map Var i) (map Var e))

instance Read' Func where
  read' f = run f $ do
              modify (strip "Func")
              n     <- state (procWord id)
              as    <- state unparens
              let ars = map read' (fromSeps ',' as)
              ss <- state unparens
              let sts = map read' (fromSeps ',' ss)
              return (Func (Var n) ars sts)

instance Read' Stmt where
  read' stmt | isPrefixOf "Assign" stmt = run stmt $ do
                                            modify (strip "Assign")
                                            v <- state (procWord id)
                                            e <- state (procParens read')
                                            return (Assign (Var v) e)
             | isPrefixOf "Alloc" stmt  = run stmt $ do
                                            modify (strip "Alloc")
                                            v <- state (procWord id)
                                            e <- state (procParens read')
                                            return (Alloc (Var v) e)
             | isPrefixOf "Return" stmt = run stmt $ do
                                            modify (strip "Return")
                                            e <- state (procParens read')
                                            return (Return e)
             | otherwise                = error $ "Couldn't match stmt " ++ stmt

instance Read' Exp where
  read' e | isPrefixOf "Int"  e = Int  (read $ strip "Int" e)
          | isPrefixOf "Bool" e = Bool (read $ strip "Bool" e)
          | isPrefixOf "Add"  e = run e $ do
                                    modify (strip "Add")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Add e0 e1)

          | isPrefixOf "Sub"  e = run e $ do
                                    modify (strip "Sub")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Sub e0 e1)

          | isPrefixOf "Mul"  e = run e $ do
                                    modify (strip "Mul")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Mul e0 e1)

          | isPrefixOf "Div"  e = run e $ do
                                    modify (strip "Div")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Div e0 e1)

          | isPrefixOf "Not"  e = run e $ do
                                    modify (strip "Not")
                                    e0 <- state (procParens read')
                                    return (Not e0)

          | isPrefixOf "And"  e = run e $ do
                                    modify (strip "And")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    -- XXX Bug!
                                    return (And e1 e0)
          | isPrefixOf "Or"  e = run e $ do
                                    modify (strip "Or")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    -- XXX Bug!
                                    return (And e1 e0)
          | otherwise           = error $ "Couldn't match exp " ++ e

--------------------------------------------------------------------------------

run :: s -> State s a -> a
run e m = (flip evalState) e m

-- strip a prefix and a space from a string.  Return the remainder of the
-- string.
strip :: String -> String -> String
strip pre str = case stripPrefix pre str of
  Nothing   -> error $ "Couldn't strip " ++ pre ++ " from " ++ str
  Just rst  -> if null rst then rst else tail rst

-- Strip the next word.
stripWord :: String -> (String, String)
stripWord str = let strs = words str in
                (head strs, unwords (tail strs))


procWord :: (String -> a) -> String -> (a, String)
procWord = runProc stripWord

-- Return a prefix inside parens and the remainder of a string.
unparens :: String -> (String, String)
unparens ('(':str) = unparens' (1::Integer) [] str
  where
  unparens' n s ('(':r) = unparens' (n+1) ('(':s) r
  unparens' n s (')':r) | n == 1    = (reverse s, strip "" r)
                        | otherwise = unparens' (n-1) (')':s) r
  unparens' _ _ []      = error $ "End of string reached in unparens"
  unparens' n s (c:r)   = unparens' n (c:s) r
unparens str = error $ "Unparsens couldn't parse " ++ str

procParens :: (String -> a) -> String -> (a, String)
procParens = runProc unparens

-- Parse up to a sep
fromSep :: Char -> String -> (String, String)
fromSep sep str = let pre  = takeWhile (/= sep) str in
                  let post = drop (length pre + 1) str in
                  (pre, post)

fromSeps :: Char -> String -> [String]
fromSeps _ []  = []
fromSeps sep str = let (a, b)  = fromSep sep str in
                   let as = fromSeps sep b in
                   a:as

runProc :: (String -> (String, String))
        -> (String -> a)
        -> String
        -> (a, String)
runProc t f s = let (a, b) = t s in (f a, b)

--------------------------------------------------------------------------------

size :: Lang -> Int
size (Lang m f) = sumit sizem m + sumit sizef f
  where
  sizem (Mod is es) = length is + length es
  sizef (Func _ as sts) = sumit sizee as + sumit sizes sts
  sizes stmt = case stmt of
    Assign _ e -> 1 + sizee e
    Alloc _ e  -> 1 + sizee e
    Return e   -> 1 + sizee e
  sizee e = case e of
    Int _       -> 1
    Bool _      -> 1
    Add e0 e1   -> 1 + sizee e0 + sizee e1
    Sub e0 e1   -> 1 + sizee e0 + sizee e1
    Mul e0 e1   -> 1 + sizee e0 + sizee e1
    Div e0 e1   -> 1 + sizee e0 + sizee e1
    Not e0      -> 1 + sizee e0
    And e0 e1   -> 1 + sizee e0 + sizee e1
    Or e0 e1    -> 1 + sizee e0 + sizee e1
  sumit sz ls = sum (map sz ls)

--------------------------------------------------------------------------------

prop_parse :: Lang -> Bool
prop_parse e = read' (show' e) == e

main :: IO ()
main = do
  Lean.check prop_parse
  check prop_parse
