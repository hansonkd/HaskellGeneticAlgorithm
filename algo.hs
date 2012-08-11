{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GenAlgo.Data
import GenAlgo.Data.DefaultChromosomes
import GenAlgo.Algo

import Data.Map (fromList)
import Data.Typeable

data Open = Open deriving (Show)


instance ValueChromosome Open where
    evaluate p a = fromIntegral $ open (unpackEnvironment p defaultEnvironment)
    isTerminalVal _ = True

openChromosome :: Stochastic (AlgoParams -> Stochastic GenericValue)
openChromosome = return (\params -> return $ packValue Open)


data MyEnvironment = MyEnvironment { open  :: Int
                                   , close :: Int
                                   } deriving (Show, Typeable)

myParams = AlgoParams { depth        = 5
                      , logicList    = []
                      , functionList = [ (intChromosome (0, 10),  1)
                                       , (addChromosome,          1)
                                       , (subtractChromosome,     1)
                                       , (multiplyChromosome,     1)
                                       , (divideChromosome,       1)
                                       , (openChromosome,         5)
                                       ]
                      }

defaultEnvironment :: MyEnvironment
defaultEnvironment = MyEnvironment { open = 0, close = 0 }

testEnvironment :: GenericEnvironment
testEnvironment = packEnvironment $ MyEnvironment { open = 5
                                                  , close = 8
                                                  }

main = do
    a <- runAlgo myParams (testEnvironment)
    print a