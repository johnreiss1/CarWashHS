module Main where

import CarWash
import Control.Monad.State.Lazy

main :: IO ()
main = do
  putStrLn "\n\nCar Wash Simulation!\n\n"
  putStrLn "Enter the Max Number of Cars in the Queue:"
  maxnS  <- getLine 
  putStrLn "Enter the time to Process a Car"
  proctS <- getLine
  putStrLn "Enter the Arrival Interval for other cars"
  arrtS  <- getLine
  putStrLn "Enter the Number of Times to Simulate the Car Wash"
  nsimS  <- getLine
  putStr "\n\n"
  let
    carWash = mkCarWash (read maxnS) (read proctS) (read arrtS)
    out = filterJust (runStateMachine (read nsimS) carWash carWashSimulation)
    waitTS = waitTimes out
    maxW = maximum waitTS
  putStr $ "Max Wait Time: " ++ show maxW ++ "\n"
  putStr $ "Average Wait Time: " ++ show (average waitTS) ++ "\n"

testCarWash :: CarWashState Integer
testCarWash = mkCarWash 2 1 2

runStateMachine :: Int -> s -> State s t -> [t]
runStateMachine n initial statefn
  | n <= 0 = []
  | otherwise = res : runStateMachine (n - 1) new statefn
  where
    (res, new) = runState statefn initial

filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust (x:xs) =
  case x of
    Nothing  -> filterJust xs
    Just val -> val : filterJust xs

waitTimes :: [CarState a] -> [a]
waitTimes = fmap waitT

procTimes :: [CarState a] -> [a]
procTimes = fmap procdT


average :: Integral a => [a] -> Int
average xs = fromIntegral (sum xs) `div` length xs
