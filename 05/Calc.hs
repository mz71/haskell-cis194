{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import qualified StackVM
import StackVM (Program, execute)
import qualified Data.Map as Map
import Control.Applicative

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e f) = eval e + eval f
eval (Mul e f) = eval e * eval f

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)


-- Exercise 3

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit i = i
  add i j = i + j
  mul i j = i * j

instance Expr Bool where
  lit b = b > 0
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
    lit i = MinMax i
    add (MinMax i) (MinMax j) = MinMax (max i j)
    mul (MinMax i) (MinMax j) = MinMax (min i j)

instance Expr Mod7 where
    lit i = realMod7 i
        where
        realMod7 :: Integer -> Mod7
        realMod7 i  | i > 0 = Mod7 (i `mod` 7)
                    | otherwise = realMod7 (i + 7)
    add (Mod7 i) (Mod7 j) = Mod7 ((i + j) `mod` 7)
    mul (Mod7 i) (Mod7 j) = Mod7 ((i * j) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr Program where
    lit i = [StackVM.PushI i]
    add p r = p ++ r ++ [StackVM.Add]
    mul p r = p ++ r ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


run :: String -> Either String StackVM.StackVal
run s = case compile s of Nothing -> Left "compilation error"
                          Just p -> execute [] p


-- Exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (Map.Map String Integer -> Maybe Integer) where
  var s = Map.lookup s

instance Expr (Map.Map String Integer -> Maybe Integer) where
  lit i = const (Just i)
  add expr1 expr2 = \ dict -> liftA2 (+) (expr1 dict) (expr2 dict)
  mul expr1 expr2 =  \ dict -> liftA2 (*) (expr1 dict) (expr2 dict) 

withVars :: [(String, Integer)]
    -> (Map.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ Map.fromList vs   
