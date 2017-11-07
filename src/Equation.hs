module Equation
    ( Equation(..)
    , reduceEquation
    , evaluateEquation
    , makeEquation
    ) where

import qualified Data.Vector.Generic as V
import Control.Monad.Mersenne.Random
import Control.Monad (liftM)

data Equation a = Plus  (Equation a) (Equation a)
                | Times (Equation a) (Equation a)
                | Minus (Equation a) (Equation a)
                | Div   (Equation a) (Equation a)
                | Input Int
                | Var   Int
                | Const a

instance Show a => Show (Equation a) where
    show (Const x)  = show x
    show (Var i)    = "V" ++ (show i)
    show (Input i)  = "I" ++ (show i)
    show (Plus a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
    show (Minus a b)= "(" ++ (show a) ++ " - " ++ (show b) ++ ")"
    show (Times a b)= "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
    show (Div a b)  = "(" ++ (show a) ++ " / " ++ (show b) ++ ")"

instance (Eq a) => Eq (Equation a) where
    (Plus  a1 b1) == (Plus  a2 b2) = ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
    (Times a1 b1) == (Times a2 b2) = ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
    (Minus a1 b1) == (Minus a2 b2) = (a1 == a2) && (b1 == b2)
    (Div   a1 b1) == (Div   a2 b2) = (a1 == a2) && (b1 == b2)
    (Const a)     == (Const b)     = a == b
    (Input a)     == (Input b)     = a == b
    (Var   a)     == (Var   b)     = a == b
    _             == _             = False

evaluateEquation :: (V.Vector v a, Fractional a) => v a -> v a -> Equation a -> a
evaluateEquation _ _ (Const x)          = x
evaluateEquation _ vars (Var i)         = vars V.! i
evaluateEquation input _ (Input i)      = input V.! i
evaluateEquation input vars (Plus a b)  = (evaluateEquation input vars a) + (evaluateEquation input vars b)
evaluateEquation input vars (Times a b) = (evaluateEquation input vars a) * (evaluateEquation input vars b)
evaluateEquation input vars (Minus a b) = (evaluateEquation input vars a) - (evaluateEquation input vars b)
evaluateEquation input vars (Div a b)   = (evaluateEquation input vars a) / (evaluateEquation input vars b)

reduceEquation :: (Fractional a, Eq a) => Equation a -> Equation a
reduceEquation (Plus a b) = reduceEquation' (Plus (reduceEquation a) (reduceEquation b))
reduceEquation (Times a b)
    | a == (Const 0.0) || b == (Const 0.0) = Const 0.0
    | otherwise = reduceEquation' (Times (reduceEquation a) (reduceEquation b))
reduceEquation (Div a b)
    | a == (Const 0.0) = Const 0.0
    | otherwise = reduceEquation' (Div (reduceEquation a) (reduceEquation b))
reduceEquation (Minus a b)
    | a == b = Const 0.0
    | otherwise = reduceEquation' (Minus (reduceEquation a) (reduceEquation b))
reduceEquation x = reduceEquation' x

-- Internal, non-recursive, reduction function
reduceEquation' :: (Fractional a, Eq a) => Equation a -> Equation a
reduceEquation' (Plus  (Const a) (Const b)) = Const (a + b)
reduceEquation' (Times (Const a) (Const b)) = Const (a * b)
reduceEquation' (Minus (Const a) (Const b)) = Const (a - b)
reduceEquation' (Div   (Const a) (Const b)) = Const (a / b)
reduceEquation' x@(Plus a b)
    | a == b    = (Times (Const 2.0) b)
    | otherwise = x
reduceEquation' x@(Times a b)
    | a == (Const 0.0) || b == (Const 0.0) = Const 0.0
    | otherwise = x
reduceEquation' (Div (Const 0.0) _)      = Const 0.0
reduceEquation' x@(Minus a b)
    | a == b = Const 0.0
    | otherwise = x
reduceEquation' x = x

getTerminal :: Int -> Rand (Equation Double)
getTerminal numInputs = do
  constant <- getBool
  if constant then
      fmap Const $ fmap (* 1000) $ getDouble
  else
      fmap Input $ fmap (`mod` numInputs) getInt

getNonTerminal :: Rand (Equation a -> Equation a -> Equation a)
getNonTerminal = do
  x <- fmap (`mod` 4) getInt
  return $ case x of
             0 -> Plus
             1 -> Times
             2 -> Minus
             3 -> Div

-- Generate a new equation using the ramped half and half method (sort-of)
makeEquation :: Int -> Int -> Rand (Equation Double)
makeEquation numInputs depth = do
  g <- getBool
  if g then grow numInputs depth else full numInputs depth

-- TODO: Abstract this pattern
grow :: Int -> Int -> Rand (Equation Double)
grow numInputs 0 = getTerminal numInputs
grow numInputs depth = do
  terminal <- getBool
  if terminal
  then getTerminal numInputs
  else do -- Pretty sure there is cleaner way to do this.
    left <- grow numInputs (depth - 1)
    right <- grow numInputs (depth - 1)
    nt <- getNonTerminal
    return $ nt left right

full :: Int -> Int -> Rand (Equation Double)
full numInputs 0     = getTerminal numInputs
full numInputs depth = do
  left  <- full numInputs (depth - 1)
  right <- full numInputs (depth - 1)
  nt <- getNonTerminal
  return $ nt left right

