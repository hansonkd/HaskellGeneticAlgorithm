{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}

module GenAlgo.Data where



import Control.Monad.Random
import Data.Functor ((<$>))
import Control.Monad

import Data.Dynamic

import Data.Map (Map, lookup)
import Prelude hiding (lookup)

class Show b => BooleanChromosome b where
    evalBool       ::  GenericEnvironment -> b -> Bool


class Show b => ValueChromosome b where
    evaluate      :: GenericEnvironment -> b -> Double


type Stochastic = Rand StdGen
type EnvironmentMap = Map String Double
type GenericEnvironment = Dynamic

data GenericBoolean       = forall a . BooleanChromosome a => GenericBoolean a
data GenericValue         = forall a . ValueChromosome   a => GenericValue   a

data AlgoParams = AlgoParams   { depth        :: Int
                               , logicList    :: [(Stochastic (AlgoParams -> Stochastic GenericBoolean, Bool), Int)]
                               , functionList :: [(Stochastic (AlgoParams -> Stochastic GenericValue, Bool),   Int)]
    } 


instance BooleanChromosome GenericBoolean where
    evalBool p (GenericBoolean a) = evalBool p a

instance Show GenericBoolean where
    show (GenericBoolean a) = show a

instance ValueChromosome GenericValue where
    evaluate b (GenericValue a) = evaluate b a

instance Show GenericValue where
    show (GenericValue a) = show a

fillValue :: AlgoParams -> Stochastic GenericValue
fillValue algoParams@AlgoParams { depth = cDepth } = do
	(valGenerator, isTerminal) <- join $ weightedUniform $ functionList algoParams
	if cDepth > 0
		then valGenerator algoParams {depth = cDepth - 1}
		else if isTerminal
			then valGenerator algoParams { depth = 0 }
			else fillValue algoParams

fillValueParams :: AlgoParams -> Stochastic (GenericValue, GenericValue)
fillValueParams algoParams@AlgoParams { depth = cDepth } = do
    val1 <- fillValue algoParams { depth = cDepth - 1 }
    val2 <- fillValue algoParams { depth = cDepth - 1 }
    return (val1, val2)

packEnvironment :: Typeable a => a -> GenericEnvironment
packEnvironment = toDyn

unpackEnvironment :: Typeable a => GenericEnvironment -> a -> a
unpackEnvironment = fromDyn

packValue :: ValueChromosome a => a -> GenericValue
packValue = GenericValue

packBool :: BooleanChromosome a => a -> GenericBoolean
packBool = GenericBoolean

weightedUniform :: [(a, Int)] -> Stochastic a
weightedUniform xs = do
    uniform $ concat $ map (\(dist, elems) -> take elems (repeat dist)) xs

uniform :: [a] -> Stochastic a
uniform xs = do
    ix <- getRandomR (0, length xs - 1)
    return (xs !! ix)