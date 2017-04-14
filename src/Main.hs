module Main where

-- import           System.Console.GetOpt
import           System.Random
-- import           Text.Read
import           Data.Maybe
import           System.Environment (getArgs, lookupEnv)

import           System.Directory   (createDirectoryIfMissing)

import           Activity

main :: IO ()
main = do
    args <- getArgs
    g    <- newStdGen
    -- | Generate random activities and join them into a single string with newlines
    -- let n = read $ fromMaybe "1" $ listToMaybe args :: Int
    let output
          | argc == 0 = "No arguments given, assuming "
                        ++ show numberArg
                        ++ " activity.\n"
                        ++ getAcList
          | argc == 1 = getAcList
          | argc == 2 = "2 arguments given. This does nothing right now."
          | argc >= 3 = "Too many arguments given."
          | otherwise = "Something is not right with the arguments."
            where
            argc              = length args
            getAcList         = unlines $ map show $ acList numberArg
            acList n          = randomActivities n activities g
            numberArg         = read $ fromMaybe "1" $ listToMaybe args :: Int
    putStrLn output

data Flag = Verbose | Version | Help
          | AddNew Activity
          | Remove Activity
            deriving (Show)

mkActiList :: Int -> IO ()
mkActiList n = do
    g <- newStdGen
    let actis = randomActivities n activities g
    let actiList = unlines $ map show actis
    putStrLn actiList



-- options :: [OptDescr Flag]
-- options = [ Option ['v'] ["verbose"] (NoArg Verbose) "Add a new activity"
--           , Option ['a'] ["add", "new"] (ReqArg addNew "") "Add a new activity"
--           ]

-- addNew :: String -> Flag
-- addNew acString = AddNew $ activities !! 0

fileName :: FilePath
fileName = "activities.txt"

appendNew :: Activity -> IO ()
appendNew ac = do
    dataDir <- lookupEnv "XDG_DATA_DIR"
    let saveDir = fromMaybe "$HOME/.local/share/" dataDir ++ "actigen/" :: FilePath
    let filePath = saveDir ++ fileName
    createDirectoryIfMissing False saveDir
    appendFile filePath (show ac)
