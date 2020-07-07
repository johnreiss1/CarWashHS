module CarWash
  ( CarState ()
  , CarWashState ()
  , mkCarWash
  , carWashSimulation
  , waitT
  , procdT
  ) where

import Control.Monad
import Control.Monad.State.Lazy

data CarState a = Car
  { waitT  :: a
  , procdT :: a
  } deriving (Show, Read)

data CarWashState a = CarWash
  { maxN  :: a
  , procT :: a
  , arrT  :: a
  , presT :: a
  , presP :: Maybe (CarState a)
  , waitP :: [CarState a]
  } deriving (Show, Read)

type CarWashOutput a = Maybe (CarState a)

mkCarWash :: Integral a
  => a -> a -> a
  -> CarWashState a
mkCarWash n t s =
  CarWash n t s 0 Nothing []

addWaitT :: Num a
  => a -> CarState a
  -> CarState a
addWaitT dt car =
  car{ waitT = dt + waitT car }

addProcT :: Num a
  => a -> CarState a
  -> CarState a
addProcT dt car =
  car{ procdT = dt + procdT car }

addCarWashT :: Num a
  => a -> CarWashState a
  -> CarWashState a
addCarWashT dt carw =
  carw
  { presT = dt + t
  , presP = fmap (addWaitT dt . addProcT dt) p
  , waitP = fmap (addWaitT dt) q
  } where 
    t = presT carw
    p = presP carw
    q = waitP carw

readyProcess :: Ord a
  => CarWashState a
  -> Bool
readyProcess state =
  case presP state of
    Nothing -> False
    Just c -> procdT c >= procT state

readyAdvance :: Integral a
  => CarWashState a
  -> Bool
readyAdvance state =
  t `mod` interval == 0
  where
    t = presT state
    interval = procT state

addQueue :: Integral a
  => CarWashState a
  -> CarWashState a
addQueue state =
  if toInteger (length queue) + 1 > toInteger (maxN state)
  then state
  else state { waitP = queue ++ [Car 0 0] }
  where
    queue = waitP state

advanceQueue :: Integral a
  => CarWashState a
  -> CarWashState a
advanceQueue state =
  case presP state of
    Nothing ->
      case waitP state of
        (c:cs) -> state { presP = Just c, waitP = cs }
        _      -> state
    _ -> state
      

processCarWash :: (Integral a, Monad m)
  => a -> CarWashState a
  -> m (CarWashOutput a, CarWashState a)
processCarWash dt statei = pure $
  case (readyProcess state, readyAdvance state) of
    (True, True)   ->
      (presP state
      , (addCarWashT dt . addQueue) state { presP = Nothing }
      )
    (True, False)  ->
      (presP state
      , addCarWashT dt state { presP = Nothing }
      )
    (False, True)  ->
      (Nothing
      , (addQueue . addCarWashT dt) state
      )
    (False, False) ->
      (Nothing
      , addCarWashT dt state
      )
  where
    state = advanceQueue statei

carWashSimulation :: Integral a
  => State (CarWashState a) (CarWashOutput a)
carWashSimulation = StateT (processCarWash 1)
