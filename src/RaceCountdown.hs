{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module RaceCountdown(
numberOfSlackDays,
daysTilRace,
runsRemaining,
Run,
findCurrentRun,
addNewRun
)
where

import Control.Applicative
import Data.Maybe
import Data.Text
import qualified Data.Time as T
import Database.MongoDB as M
import Control.Monad.IO.Class

type Week = Int
newtype Run = Run (Week, Int)

percentageDaysRunning :: Run -> IO Double
percentageDaysRunning run =  (/) <$>  return ( fromIntegral $ runsRemaining run) <*> fromIntegral <$> daysTilRace 

numberOfSlackDays :: Run -> IO Integer
numberOfSlackDays run = do
  dl <- daysTilRace
  let r = toInteger $ runsRemaining run
  let s = floor $ (4 / 3) * toRational r
  return (dl - (s + r))

daysTilRace :: IO Integer
daysTilRace = daysUntil deadline

daysUntil :: T.Day -> IO Integer
daysUntil x = do
  c <- T.getCurrentTime
  return $ T.diffDays x $ T.utctDay c


deadline :: T.Day
deadline = T.fromGregorian 2014 09 14

runsRemaining :: Run -> Int 
runsRemaining (Run(w , r)) = numberOfRunsInC25K - (r + 3 * (w - 1))

numberOfRunsInC25K :: Int
numberOfRunsInC25K = 8 * 3

findCurrentRun :: Action IO Run
findCurrentRun = runFromDoc <$> findOne latestRunQuery

latestRunQuery :: Query
latestRunQuery = (select [] runCollectionF) {project = [weekF =: 1, runF =: 1], sort = [dateF =: -1]}

runFromDoc :: Maybe Document -> Run
runFromDoc md = fromMaybe (Run (0,0)) $ do
  d <- md
  w <- M.lookup weekF d
  r <- M.lookup runF d
  return $ Run (w, r)

addNewRun :: (Int, Int) -> Action IO ()
addNewRun (w,i) = do
    d <- liftIO T.getCurrentTime
    insert_ runCollectionF [runF =: i, weekF =: w, dateF =: d]
                  


runF :: Text
runF = "run"
weekF :: Text
weekF = "week"
dateF :: Text
dateF = "date"

runCollectionF :: Text
runCollectionF = "runs"
