module RaceCountdown(
numberOfSlackDays,
daysTilRace,
runsRemaining
)
where

import Data.Time as T
import Control.Applicative

type Week = Int
type Run = Int

percentageDaysRunning :: IO Double
percentageDaysRunning =  (/) <$>  return ( fromIntegral runsRemaining) <*> fromIntegral <$> daysTilRace 

numberOfSlackDays :: IO Integer
numberOfSlackDays = do
  dl <- daysTilRace
  let r = toInteger runsRemaining
  return (dl - (2 * r))

daysTilRace :: IO Integer
daysTilRace = daysUntil deadline

daysUntil :: Day -> IO Integer
daysUntil x = do
  c <- T.getCurrentTime
  return $ T.diffDays x $ utctDay c


deadline :: Day
deadline = fromGregorian 2014 09 14

runsRemaining :: Int
runsRemaining = runsRemaining' currentRun

runsRemaining' :: (Week, Run) -> Int 
runsRemaining' (w , r) = numberOfRunsInC25K - (r + 3 * (w - 1))

numberOfRunsInC25K :: Int
numberOfRunsInC25K = 8 * 3

currentRun :: (Week , Run)
currentRun = (4 , 2)
