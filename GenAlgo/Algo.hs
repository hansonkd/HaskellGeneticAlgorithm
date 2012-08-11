module GenAlgo.Algo where

import GenAlgo.Data
import GenAlgo.Data.DefaultChromosomes
import Control.Monad.Random
import Control.Monad

runAlgo :: AlgoParams -> GenericEnvironment -> IO Double
runAlgo algoParams ae = do
    x <- evalRandIO $ fillValue algoParams
    print x
    return $ evaluate ae x