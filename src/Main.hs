{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.ByteString       as ByteString (writeFile)
import qualified Data.ByteString.Char8 as ByteString (pack, putStr)
import qualified Data.HashMap.Strict   as HashMap (insert, lookup)
import qualified Data.Text             as Text (pack)
import qualified Data.Vector           as Vector (cons)
import           Data.Yaml             hiding (Parser)
import           Options.Applicative
import           System.Directory
import           System.Exit           (exitFailure)
import           System.FilePath
import           System.IO             (hPutStrLn, stderr)

data Command = CommandAdd String String
             | CommandGet String
             | CommandSet String String
  deriving(Show)

data CommonOptions = CommonOptions { coptStdout :: Bool
                                   , coptPath   :: Maybe String
                                   }
  deriving(Show)

data Options = Options (Command, CommonOptions)
  deriving(Show)

commonOptions :: Parser CommonOptions
commonOptions = CommonOptions
          <$> switch (long "stdout"
                      <> short 'o'
                      <> help "Output data to standard output")
          <*> optional (strOption (long "file"
                                   <> short 'f'
                                   <> help "Input file '-' for stdin"
                                   <> metavar "FILE"
                                  ))

options :: Parser Options
options = Options <$> subparser
              (command "add" (info' cmdAdd
                              (progDesc "Add an item to a list"))
               <> command "get" (info' cmdGet
                                 (progDesc "Get a key"))
               <> command "set" (info' cmdSet
                                 (progDesc "Set a key")))
  where
    info' p = info ((,) <$> p <*> commonOptions)
    cmdAdd = CommandAdd
        <$> argument str (metavar "KEY")
        <*> argument str (metavar "VALUE")
    cmdGet = CommandGet
        <$> argument str (metavar "KEY")
    cmdSet = CommandSet
        <$> argument str (metavar "KEY")
        <*> argument str (metavar "VALUE")

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options) (fullDesc <> progDesc "Manipulate YAML")

run :: Options -> IO ()
run (Options (optCommand, CommonOptions coptStdout Nothing)) = do
    cwd <- getCurrentDirectory
    fs <- getDirectoryContents cwd
    case filter ((== ".yaml") . takeExtension) fs of
        [] -> error "Couldn't find a .yaml file in the current directory"
        (f:_) -> run (Options (optCommand, CommonOptions coptStdout (Just f)))
run (Options (optCommand, CommonOptions coptStdout (Just coptPath))) = do
    einput <- decodeFileEither coptPath
        :: IO (Either ParseException Value)
    case einput of
        Left e -> do
            hPutStrLn stderr ("Failed to parse " ++ coptPath)
            hPutStrLn stderr (prettyPrintParseException e)
            exitFailure
        Right input -> do
            r <- case optCommand of
                CommandAdd k v -> runAdd input k v
                CommandSet k v -> runSet input k v
                CommandGet k -> runGet input k
            finish r
  where
    finish :: Maybe Value -> IO ()
    finish Nothing = return ()
    finish (Just o) | coptStdout = ByteString.putStr (encode o)
    finish (Just o) = ByteString.writeFile coptPath (encode o)

runAdd :: Monad m => Value -> String -> String -> m (Maybe Value)
runAdd (Object o) k v = case HashMap.lookup (Text.pack k) o of
    Nothing -> error "Key is not there"
    Just (Array a) -> case decode (ByteString.pack v) of
        Nothing -> error "Can't encode value as YAML"
        Just v' -> do
            let a' = Vector.cons v' a
            return $ Just (Object (HashMap.insert (Text.pack k) (Array a') o))
    _ -> error "Key is not an Array"
runAdd _ _ _ = error "Not implemented"

runGet :: Value -> String -> IO (Maybe a)
runGet (Object o) k = case HashMap.lookup (Text.pack k) o of
    Nothing -> error "Key is not here"
    Just v -> do
        ByteString.putStr (encode v)
        return Nothing
runGet _ _ = error "Not implemented"

runSet :: t
runSet = error "Not implemented"
