module Main where

import           Data.Maybe
import           System.Environment (getArgs, lookupEnv)
import           System.Random

import           System.Directory   (createDirectoryIfMissing)

import           Activity

main :: IO ()
main = do
    args <- getArgs
    g    <- newStdGen
    -- | Generate random activities and join them into a single string with newlines
    let n = read $ fromMaybe "1" $ listToMaybe args :: Int
    let output
          | argc == 0 = "No arguments given, assuming "
                        ++ show n
                        ++ " activity.\n"
                        ++ mkActiList n g
          | argc == 1 = mkActiList n g
          | argc == 2 = "2 arguments given. This does nothing right now."
          | argc >= 3 = "Too many arguments given."
          | otherwise = "Something is not right with the arguments."
          where
            argc              = length args
    putStrLn output

data Flag = Verbose | Version | Help
          | AddNew Activity
          | Remove Activity
            deriving (Show)

mkActiList :: Int -> StdGen -> String
mkActiList n g = unlines $ map show actis
  where
    actis = randomActivities n activities g

newActiList :: Int -> IO String
newActiList n = do
    g <- newStdGen
    let actis = randomActivities n activities g
    let actiList = unlines $ map show actis
    return actiList

fileName :: FilePath
fileName = "activities.txt"

appendNew :: Activity -> IO ()
appendNew ac = do
    dataDir <- lookupEnv "XDG_DATA_DIR"
    let saveDir = fromMaybe "$HOME/.local/share/" dataDir ++ "actigen/" :: FilePath
    let filePath = saveDir ++ fileName
    createDirectoryIfMissing False saveDir
    appendFile filePath (show ac)

-- addNew :: String -> Flag
-- addNew acString = AddNew $ activities !! 0

-- options :: [OptDescr Flag]
-- options = [ Option ['v'] ["verbose"] (NoArg Verbose) "Add a new activity"
--           , Option ['a'] ["add", "new"] (ReqArg addNew "") "Add a new activity"
--           ]
