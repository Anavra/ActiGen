module Activity where

import System.Random

import Test.QuickCheck

type Tag = String
type Description = String

activities = [ (Activity "Draw"            [ "Artistic","Relaxing"])  :: Activity
             , (Activity "Play the piano"  [ "Artistic", "Relaxing"]) :: Activity
             , (Activity "Game"            [ "Mind", "Exciting"])     :: Activity
             ]

data Activity = Activity Description [Tag] deriving (Show)

randomActivity :: [Activity] -> StdGen -> Activity
randomActivity acts stdGen = (acts !!) . fst $ randomR (0, length activities - 1) stdGen

randomActivities :: [Activity] -> Int -> StdGen -> [Activity]
randomActivities acs c g
    | c <= 0    = error $ "Unable to make a list of " ++ (show c) ++ " activities."
    | c == 1    = [randomActivity acs g]
    | otherwise = (rAc:rAcs)
          where rAc  = randomActivity acs g
                rAcs = randomActivities acs (c - 1) nG
                nG = snd $ next g

-- prop_randomActivity :: Int -> Bool
-- prop_randomActivity x = randomActivity activities x !=
