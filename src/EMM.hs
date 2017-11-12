module EMM
  ( Model(..)
  , evaluateModel
  , setInput
  , EMM.recombine
  ) where

import Equation
import qualified Data.Vector as V
import Control.Monad.Mersenne.Random
import Control.Monad

data Model = Model { variables :: V.Vector Double
                   , input     :: V.Vector Double
                   , output    :: V.Vector Double
                   , state     :: V.Vector (Equation Double)
                   , equations :: V.Vector (Equation Double)
                   } deriving Show

evaluateModel :: Model -> Model
evaluateModel model = model { variables = V.map (evaluateEquation (input model) (variables model)) $ state model
                            , output    = V.map (evaluateEquation (input model) (variables model)) $ equations model
                            }

setInput :: V.Vector Double -> Model -> Model
setInput newInput model = model { input = newInput }

addVariable :: Model -> Rand Model
addVariable model = do
  -- Select a random equation to add the new variable to. This is done
  -- in the paper, but I wonder if it is really necessary. I'm leaving
  -- it out for now, since it is not so easy to implement. (also seems
  -- like a drastic mutation, not adding it would allow for the
  -- accumulation of mutations in the unused equation---increasing the
  -- diversity of the population)
  --
  -- let numEquations = (V.size $ state model) + (V.size $ equations model)
  -- eq <- fmap (`mod` numEquations) getInt
  -- let equation = if eq >= V.size $ state model then (state model) V.! eq else (equations model) V.! (eq - V.size $ state model)
  init <- getDouble
  depth <- fmap (`mod` 2) getInt
  stateEquation <- makeEquation (V.length $ input model) ((V.length $ state model)+1) depth
  return $ model { variables = V.snoc (variables model) init
                 , state     = V.snoc (state model) stateEquation }

makeModel :: Int -> Int -> Rand Model
makeModel numInputs numOutputs = do
  initialOutput <- fmap V.fromList $ replicateM numOutputs getDouble
  eqs <- fmap V.fromList $ replicateM numOutputs $ makeEquation numInputs 0 3
  return $ Model { variables = V.fromList []
                 , input = V.fromList []
                 , state = V.fromList []
                 , equations = eqs
                 , output = initialOutput }

--selectEquation :: V.Vector (Equation a) -> Rand (Equation a)
--selectEquation eqs =
--  liftM (eqs V.!) $ fmap (`mod` V.length eqs) getInt

selectEquation :: Model -> Rand (Equation Double)
selectEquation m = do
  let numEquations = V.length . equations $ m
  let numState = V.length . state $ m
  i <- fmap (`mod` (numEquations + numState)) getInt
  if i >= numEquations
  then return $ (state m) V.! (i - numEquations)
  else return $ (equations m) V.! i

recombine :: Double -> Double -> Model -> Model -> Rand Model
recombine pTreeLevel pTree m1 m2 = do
  -- combine the vectors for state and equations from m1 into a single vector
  -- for each vector recombine it with a random equation from m2 with probability pTree
  let allTrees = (equations m1) V.++ (state m1)
  (equations', state') <- fmap (V.splitAt . V.length . equations $ m1) $ mapM doRecombine allTrees
  return $ m1 { equations = equations', state = state' }
  where doRecombine eq1 = do
          p <- getDouble
          if p < pTree
          then do
            eq <- selectEquation m2
            Equation.recombine eq1 eq
          else return eq1
