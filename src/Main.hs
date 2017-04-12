module Main where

import           System.Console.GetOpt
import           System.Random
import           System.Environment (getArgs, lookupEnv)

import           Data.Maybe         (fromMaybe)
import           System.Directory   (createDirectoryIfMissing)

import           Activity

main :: IO ()
main = do
    args <- getArgs
    g    <- newStdGen
    -- | Generate random activities and join them into a single string with newlines
    let output | argc <= 0 = "Too few arguments given."
               | argc == 1 = unlines $ map show aList
               | argc == 2 = "2 arguments given. This does nothing right now."
               -- | argc == 2 = do
               --     case arg1 of
               --     return $ addNew (args !! 1)
               | argc >= 3 = "Too many arguments given."
               | otherwise = "Something is not right with the arguments."
             where
               argc        = length args
               aList       = randomActivities activities arg1 g
               cArg        = read $ head args
               cArgDefault = 0
               arg1 | cArg >= 1 = cArg
                    | otherwise = cArgDefault
    putStrLn output

data Flag = Verbose | Version | Help
          | AddNew Activity
          | Remove Activity
            deriving (Show)

options :: [OptDescr Flag]
options = [ Option ['a'] ["add", "new"] (ReqArg addNew "") "Add a new activity"
          ]

fileName :: FilePath
fileName = "activities.txt"

addNew :: String -> Flag
addNew acString = AddNew $ activities !! 0

appendNew :: Activity -> IO ()
appendNew ac = do
    dataDir <- lookupEnv "XDG_DATA_DIR"
    let saveDir = fromMaybe "$HOME/.local/share/" dataDir ++ "actigen/" :: FilePath
    let filePath = saveDir ++ fileName
    createDirectoryIfMissing False saveDir
    appendFile filePath (show ac)
