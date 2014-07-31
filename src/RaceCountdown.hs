module RaceCountdown(
numberOfSlackDays,
daysTilRace,
runsRemaining
)
where

import Data.Time as T
import Control.Applicative

type Week = Int
type Run = (Week, Int)

percentageDaysRunning :: Run -> IO Double
percentageDaysRunning run =  (/) <$>  return ( fromIntegral $ runsRemaining run) <*> fromIntegral <$> daysTilRace 

numberOfSlackDays :: Run -> IO Integer
numberOfSlackDays run = do
  dl <- daysTilRace
  let r = toInteger $ runsRemaining run
  return (dl - (2 * r))

daysTilRace :: IO Integer
daysTilRace = daysUntil deadline

daysUntil :: Day -> IO Integer
daysUntil x = do
  c <- T.getCurrentTime
  return $ T.diffDays x $ utctDay c


deadline :: Day
deadline = fromGregorian 2014 09 14

runsRemaining :: Run -> Int 
runsRemaining (w , r) = numberOfRunsInC25K - (r + 3 * (w - 1))

numberOfRunsInC25K :: Int
numberOfRunsInC25K = 8 * 3

currentRun ::Run
currentRun = (4 , 2)
