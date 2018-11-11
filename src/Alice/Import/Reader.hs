{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Main text reading functions.
-}

module Alice.Import.Reader (readInit, readText, parse) where

import Data.List
import Control.Monad
import System.IO
import System.IO.Error
import System.Exit hiding (die)
import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class

import Alice.Data.Text.Block
import Alice.Data.Instr
import Alice.ForTheL.Base
import Alice.ForTheL.Structure
import Alice.Parser.Base
import Alice.ForTheL.Instruction
import Alice.Core.Position
import Alice.Parser.Token
import Alice.Parser.Combinators
import Alice.Parser.Primitives
import qualified Alice.Core.Message as Message
import Alice.Parser.Error




type Output a = ExceptT (String, SourcePos) IO a

-- Init file parsing

readInit :: String -> Output [Instr]
readInit "" = return []
readInit file = do
  input <- liftIO $ catch (readFile file) $ die file . ioeGetErrorString
  let tokens = tokenize (filePos file) input
      initialParserState = State () tokens noPos
  fst <$> launchParser instructionFile initialParserState

instructionFile :: Parser st [Instr]
instructionFile = after (optLL1 [] $ chainLL1 instr) eof


-- Reader loop

{-| `readText lb ts` takes a library directory and an accumulator of `Text` `ts`. Read docs for `reader` to see how this works. -}
readText :: String -> [Text] -> Output [Text]
readText pathToLibrary = reader pathToLibrary [] [State initFS noTokens noPos]

{-| 
`reader lb fs ss ts` runs a parser, assuming that the deepest element of `ts` is an instruction to load a library or a file.
The reader will recursively parse files.
      - `lb :: String` is a library directory.
      - `fs :: [String]` are the full paths of files that have already been parsed.
      - `ss :: [State FState]` is a stack of parser states to track the parser as it follows file reading instructions.
      - `ts : [Text]` is an accumulator of `Text` objects. 
-}
reader :: String -> [String] -> [State FState] -> [Text] -> Output [Text]

reader _ _ _ [TI (InStr ISread file)] | isInfixOf ".." file =
  liftIO $ die file "contains \"..\", not allowed"

reader pathToLibrary doneFiles stateList [TI (InStr ISread file)] =
  reader pathToLibrary doneFiles stateList
    [TI $ InStr ISfile $ pathToLibrary ++ '/' : file]

reader pathToLibrary doneFiles (pState:states) [TI (InStr ISfile file)]
  | file `elem` doneFiles = do
      --liftIO $ Message.outputMain Message.WRITELN (fileOnlyPos file) "already read, skipping"
      (newText, newState) <- launchParser forthel pState
      reader pathToLibrary doneFiles (newState:states) newText

reader pathToLibrary doneFiles (pState:states) [TI (InStr ISfile file)] = do
  let gfl =
        if   null file
        then getContents
        else readFile file
  input <- liftIO $ catch gfl $ die file . ioeGetErrorString
  let tokens = tokenize (filePos file) input
      st  = State ((stUser pState) { tvr_expr = [] }) tokens noPos
  (ntx, nps) <- launchParser forthel st
  reader pathToLibrary (file:doneFiles) (nps:pState:states) ntx

-- this happens when t is not a suitable instruction
reader pathToLibrary doneFiles stateList (t:restText) =
  (t:) <$> reader pathToLibrary doneFiles stateList restText

reader pathToLibrary doneFiles (pState:oldState:rest) [] = do
  let resetState = oldState {
        stUser = (stUser pState) {tvr_expr = tvr_expr $ stUser oldState}}
  (newText, newState) <- launchParser forthel resetState
  reader pathToLibrary doneFiles (newState:rest) newText

reader _ _ _ [] = return []



-- | launch a parser in the IO monad
launchParser :: Parser st a -> State st -> Output (a, State st)
launchParser parser state =
  case runP parser state of
    Error err -> throwE (show err, errorPos err)
    Ok [PR a st] -> return (a, st)

-- Service stuff

die :: String -> String -> IO a
die fileName msg = exitFailure

parse :: String -> ExceptT (String, SourcePos) IO [Text]
parse fileName = do

      let commandLine = [InStr ISfile fileName]
      initFile <- readInit (askIS ISinit "init.opt" commandLine)

      let initialOpts = initFile ++ commandLine
          revInitialOpts = reverse initialOpts

      -- parse input text
      readText (askIS ISlibr "." revInitialOpts) $ map TI initialOpts