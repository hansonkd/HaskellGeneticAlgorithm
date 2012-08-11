{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module GenAlgo.Data.DefaultChromosomes where

import GenAlgo.Data
import Data.Functor ((<$>))
import Control.Monad.Random


	-------------------------------
	-- Default Logic Chromosomes --
	-------------------------------

data Not = Not GenericBoolean            deriving (Show)
data GrT = GrT GenericValue GenericValue deriving (Show)
data LsT = LsT GenericValue GenericValue deriving (Show)

instance BooleanChromosome Not where
    evalBool p c = (not $ evalBool p c) 
	
instance BooleanChromosome GrT where
    evalBool p (GrT fr sc) = ((evaluate p fr) > (evaluate p sc))
	
instance BooleanChromosome LsT where
    evalBool p (LsT fr sc) = ((evaluate p fr) < (evaluate p sc))


	----------------------------------
	-- Default Function Chromosomes --
	----------------------------------

data ConstInt = ConstInt Double deriving (Show)
data Add      = Add (GenericValue, GenericValue) deriving (Show)
data Subtract = Subtract (GenericValue, GenericValue) deriving (Show)
data Multiply = Multiply (GenericValue, GenericValue) deriving (Show)
data Divide   = Divide (GenericValue, GenericValue) deriving (Show)

instance ValueChromosome ConstInt where
	evaluate p (ConstInt i) = i


instance ValueChromosome Add where
	evaluate p (Add (x, y)) = ((evaluate p x) + (evaluate p y))


instance ValueChromosome Subtract where
	evaluate p (Subtract (x, y)) =((evaluate p x) - (evaluate p y))


instance ValueChromosome Multiply where
	evaluate p (Multiply (x, y)) = ((evaluate p x) * (evaluate p y))


instance ValueChromosome Divide where
	evaluate p (Divide (x, y)) = ((evaluate p x) / (evaluate p y))


intChromosome :: (Int, Int) -> Stochastic (AlgoParams -> Stochastic GenericValue, Bool)
intChromosome range = return ((\_ -> packValue <$> ConstInt <$> fromIntegral <$> getRandomR range), True)
	
addChromosome :: Stochastic (AlgoParams -> Stochastic GenericValue, Bool)
addChromosome = return ((\params -> packValue <$> Add <$> (fillValueParams params)), False)

subtractChromosome :: Stochastic (AlgoParams -> Stochastic GenericValue, Bool)
subtractChromosome = return ((\params -> packValue <$> Subtract <$> (fillValueParams params)), False)

multiplyChromosome :: Stochastic (AlgoParams -> Stochastic GenericValue, Bool)
multiplyChromosome = return ((\params -> packValue <$> Multiply <$> (fillValueParams params)), False)

divideChromosome :: Stochastic (AlgoParams -> Stochastic GenericValue, Bool)
divideChromosome = return ((\params -> packValue <$> Divide <$> (fillValueParams params)), False)

