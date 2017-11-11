module Equation
    ( Equation(..)
    , reduceEquation
    , evaluateEquation
    , makeEquation
    ,
    ) where

import qualified Data.Vector.Generic as V
import Control.Monad.Mersenne.Random hiding (R)
import Control.Monad (liftM, liftM2)

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

data EqCtx a = Top
             | L (EqCtx a) (Equation a) (Equation a -> Equation a -> Equation a)
             | R (EqCtx a) (Equation a) (Equation a -> Equation a -> Equation a)

type EqZipper a = (Equation a, EqCtx a)

left :: EqZipper a -> EqZipper a
left (Plus x y, ctx)  = (x, (L ctx y Plus))
left (Times x y, ctx) = (x, (L ctx y Times))
left (Minus x y, ctx) = (x, (L ctx y Minus))
left (Div x y, ctx)   = (x, (L ctx y Div))

right :: EqZipper a -> EqZipper a
right (Plus x y, ctx)  = (x, (R ctx y Plus))
right (Times x y, ctx) = (x, (R ctx y Times))
right (Minus x y, ctx) = (x, (R ctx y Minus))
right (Div x y, ctx)   = (x, (R ctx y Div))

up :: EqZipper a -> EqZipper a
up (x, L ctx y f) = (f x y, ctx)

upmost :: EqZipper a -> EqZipper a
upmost z@(x, Top) = z
upmost z          = upmost $ up z

makeZipper :: Equation a -> EqZipper a
makeZipper eq = (eq, Top)

isTerminal :: Equation a -> Bool
isTerminal (Var _)   = True
isTerminal (Input _) = True
isTerminal (Const _) = True
isTerminal _         = False

isNonTerminal :: Equation a -> Bool
isNonTerminal = not . isTerminal

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

getTerminal :: Int -> Int -> Rand (Equation Double)
getTerminal numInputs numVars = do
  constant <- getBool
  if constant then
      fmap Const $ fmap (* 1000) $ getDouble
  else do
    x <- fmap (`mod` (numInputs+numVars)) getInt
    if x < numInputs
    then return $ Input x
    else return $ Var (x - numInputs)

getNonTerminal :: Rand (Equation a -> Equation a -> Equation a)
getNonTerminal = do
  x <- fmap (`mod` 4) getInt
  return $ case x of
             0 -> Plus
             1 -> Times
             2 -> Minus
             3 -> Div

-- Generate a new equation using the ramped half and half method (sort-of)
makeEquation :: Int -> Int -> Int -> Rand (Equation Double)
makeEquation numInputs numVars depth = do
  g <- getBool
  if g then grow numInputs numVars depth else full numInputs numVars depth

-- TODO: Abstract this pattern
grow :: Int -> Int -> Int -> Rand (Equation Double)
grow numInputs numVars 0 = getTerminal numInputs numVars
grow numInputs numVars depth = do
  terminal <- getDouble
  if terminal < 0.429 -- 3/7 chance of choosing a terminal
  then getTerminal numInputs numVars
  else do -- Pretty sure there is cleaner way to do this.
    left <- grow numInputs numVars (depth - 1)
    right <- grow numInputs numVars (depth - 1)
    nt <- getNonTerminal
    return $ nt left right

full :: Int -> Int -> Int -> Rand (Equation Double)
full numInputs numVars 0     = getTerminal numInputs numVars
full numInputs numVars depth = do
  left  <- full numInputs numVars (depth - 1)
  right <- full numInputs numVars (depth - 1)
  nt <- getNonTerminal
  return $ nt left right

-- select a node in the equation. there is a 10% chance of selecting a terminal node.
selectNode :: Equation a -> Rand (EqZipper a)
selectNode eq = do
  terminal <- fmap (< 0.1) getDouble
  if terminal then selectTerminalNode eq else selectNonTerminalNode eq

selectTerminalNode :: Equation a -> Rand (EqZipper a)
selectTerminalNode = (liftM snd) . selectTerminalNode' . makeZipper
  where selectTerminalNode' :: (EqZipper a) -> Rand (Double, EqZipper a)
        selectTerminalNode' eqz@(eq, ctx) =
          if isTerminal eq
          then do
            p <- getDouble
            return (p, eqz)
          else do
            -- look left, look right, return max
            l <- selectTerminalNode' (left eqz)
            r <- selectTerminalNode' (right eqz)
            if (fst l) > (fst r) then return l else return r

selectNonTerminalNode :: Equation a -> Rand (EqZipper a)
selectNonTerminalNode = (liftM snd) . selectNonTerminalNode' . makeZipper
  where selectNonTerminalNode' :: EqZipper a -> Rand (Double, EqZipper a)
        selectNonTerminalNode' eqz@(eq, ctx) =
          if isNonTerminal eq
          then do
            p <- getDouble
            l <- selectNonTerminalNode' (left eqz)
            r <- selectNonTerminalNode' (right eqz)
            if p > (fst l) && p > (fst r) then return (p, eqz)
              else if (fst l) > (fst r) then return l else return r
          else return (-1.0, eqz)

-- select a random subtree from eq2 and graft it over a random subtree
-- in eq1.
recombine :: Equation a -> Equation a -> Rand (Equation a)
recombine eq1 eq2 = liftM2 graft (selectNode eq1) (selectNode eq2)

graft :: EqZipper a -> EqZipper a -> Equation a
graft (eq, _) (_, ctx) = fst . upmost $ (eq, ctx)