module Test.MySolutions where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parSequence_, parTraverse, parOneOf)
import Data.Array (concat, replicate, filter, (:))
import Data.Either (Either(..))
import Data.Foldable (fold, sequence_)
import Data.Maybe (Maybe(..))
import Data.String (length, null) as String
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, attempt, message, launchAff_)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readTextFile, writeFile, writeTextFile)
import Node.Path (FilePath, basename, dirname)
import Node.Path (concat, relative, normalize) as Path

-- Note to reader: Add your solutions to this file
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 outFile = do
  file1_data <- readTextFile UTF8 file1
  file2_data <- readTextFile UTF8 file2
  writeTextFile UTF8 outFile $ file1_data <> file2_data

concatenateMany :: (Array FilePath) -> FilePath -> Aff Unit
concatenateMany files outPath = do
  fileData <- traverse (readTextFile UTF8) files
  writeTextFile UTF8 outPath $ fold fileData

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = attempt do
  fileData <- readTextFile UTF8 file
  pure $ String.length fileData

countCharacters' :: FilePath -> Aff Int
countCharacters' file = do
  fileData <- readTextFile UTF8 file
  pure $ String.length fileData

launchCount1 = launchAff_ do
    count <- countCharacters "test/data/nbChars.txt"
    log $ show count

launchCount2 = launchAff_ do
    count <- countCharacters "test/data/foof.txt"
    log $ show count

launchCount1' = launchAff_ do
    count <- countCharacters' "test/data/nbChars.txt"
    log $ show count

launchCount2' = launchAff_ do
    count <- countCharacters' "test/data/foof.txt"
    log $ show count


writeGet :: String -> FilePath -> Aff Unit
writeGet url file = do
  result <- AX.get ResponseFormat.string url
  case result of
    Left _ -> pure unit
    Right response -> writeTextFile UTF8 file response.body
  

delayArray :: Array (Aff Unit)
delayArray = replicate 100 $ delay $ Milliseconds 10.0

seqDelay :: Effect Unit
seqDelay = launchAff_ $ sequence_ delayArray

parDelay :: Effect Unit
parDelay = launchAff_ $ parSequence_ delayArray

getUrl :: String -> Aff String
getUrl url = do
  result <- AX.get ResponseFormat.string url
  pure $ case result of
    Left err -> "GET /api response failed to decode: " <> AX.printError err
    Right response -> response.body

fetchPar :: Effect Unit
fetchPar =
  launchAff_
    $ do
        let
          urls = map (\n -> "https://reqres.in/api/users/" <> show n) [ 1, 2 ]
        res <- parTraverse getUrl urls
        logShow res



concatenateManyParallel :: (Array FilePath) -> FilePath -> Aff Unit
concatenateManyParallel files outPath = do
  fileData <- parTraverse (readTextFile UTF8) files
  writeTextFile UTF8 outPath $ fold fileData


getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout timeout url = 
    let
        request = do 
            result <- AX.get ResponseFormat.string url
            pure $ case result of
                Left _ -> Nothing
                Right res -> Just res.body
        timedout = do
            _ <- delay $ Milliseconds timeout
            pure Nothing
    in
        parOneOf [request, timedout]
          

recurseFiles :: FilePath -> Aff (Array String)
recurseFiles file = do
    contents <- readTextFile UTF8 file
    case contents of
        "" -> pure [file]
        _ -> do
            let paths = split (Pattern "\n") contents
            let subPaths = map (\p -> Path.concat [dirname file, p]) paths
            contents' <- parTraverse recurseFiles subPaths
            pure $ file : concat contents'

launchRecurse = launchAff_ do
    result <- recurseFiles "test/data/tree/root.txt"
    log $ show result