{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Main text reading functions.
-}

module Alice.Import.Reader (readInit, readText) where

import Data.List
import Control.Monad
import System.IO
import System.IO.Error
import System.Exit hiding (die)
import Control.Exception

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
import Alice.Core.Message

import Debug.Trace

-- Init file parsing

readInit :: String -> IO [Instr]
readInit "" = return []
readInit file =
  do  input <- catch (readFile file) $ die file . ioeGetErrorString
      let toks = tokenize (filePos file) input ; ips = State () toks
      liftM fst $ launchParser instf ips

instf :: Parser st [Instr]
instf = after (optLL1 [] $ chainLL1 instr) eof


-- Reader loop

{-| `readText lb ts` takes a library directory and an accumulator of `Text` `ts`. Read docs for `reader` to see how this works. -}
readText :: String -> [Text] -> IO [Text]
readText lb = reader lb [] [State initFS []]
{-| 
`reader lb fs ss ts` runs a parser, assuming that the deepest element of `ts` is an instruction to load a library or a file.
The reader will recursively parse files.
      - `lb :: String` is a library directory.
      - `fs :: [String]` are the full paths of files that have already been parsed.
      - `ss :: [State FState]` is a stack of parser states to track the parser as it follows file reading instructions.
      - `ts : [Text]` is an accumulator of `Text` objects. 
-}
reader :: String -> [String] -> [State FState] -> [Text] -> IO [Text]

reader _ _ _ [TI (InStr ISread file)] | isInfixOf ".." file =
      die file "contains \"..\", not allowed"

-- Read a library file. This is treated the same as reading a file but with the `lb` file extension.
reader lb fs ss [TI (InStr ISread file)] =
      reader lb fs ss [TI $ InStr ISfile $ lb ++ '/' : file]

-- If we've already read a file don't read again, 
-- [TODO] but why is it running the first parser in the state stack again?? 
reader lb fs (ps:ss) [TI (InStr ISfile file)] | file `elem` fs =
  do  outputMain NORMAL (fileOnlyPos file) "already read, skipping"
      (ntx, nps) <- launchParser forthel ps
      reader lb fs (nps:ss) ntx
-- Read a file in the pwd.
reader lb fs (ps:ss) [TI (InStr ISfile file)] =
  do  let gfl = if null file  then hGetContents stdin
                              else readFile file
      input <- catch gfl $ die file . ioeGetErrorString
      let toks = tokenize (filePos file) input
          st  = State ((stUser ps) { tvr_expr = [] }) toks
      (ntx, nps) <- launchParser forthel st
      reader lb (file:fs) (nps:ps:ss) ntx

reader lb fs ss (t:ts) = liftM (t:) $ reader lb fs ss ts

reader lb fs (sps:ps:ss) [] =
  do  outputParser NORMAL (fileOnlyPos $ head fs) "parsing successful"
      let rps = ps {stUser = (stUser sps) {tvr_expr = tvr_expr $ stUser ps}}
      (ntx, nps) <- launchParser forthel rps
      reader lb fs (nps:ss) ntx

reader _ _ _ [] = return []

-- | launch a parser in the IO monad
launchParser :: Parser st a -> State st -> IO (a, State st)
launchParser parser st =
  case runP parser st of
    Error err -> outputParser NORMAL noPos (show err) >> exitFailure
    Ok [PR a st] -> return (a, st)
    _ -> outputParser NORMAL noPos "ambiguity error here" >> exitFailure

-- Service stuff

die :: String -> String -> IO a
die fileName st = outputMain NORMAL (fileOnlyPos fileName) st >> exitFailure
