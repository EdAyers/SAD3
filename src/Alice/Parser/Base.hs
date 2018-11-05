{-
Authors: Steffen Frerix (2017 - 2018)

Parser datatype and monad instance.
-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Alice.Parser.Base
  ( Parser(..),
    State (..),
    stPosi,
    ParseResult (..),
    Reply (..),
    runP,
    getInput,
    getPos,
    getText )
  where

import Control.Monad
import qualified Control.Applicative as Applicative
import Control.Monad.State.Class


import Alice.Parser.Token
import Alice.Parser.Error
import Alice.Core.Position

import Data.Char
import Data.List

import Debug.Trace


-- | Parser state
data State st = State {
  -- | The user state.
  stUser :: st, 
  -- | The input stream.
  stInput :: [Token] 
}

stPosi :: State st -> SourcePos
stPosi (State _ (t:ts)) = tokenPos t
stPosi (State _ [])     = EOF

-- | intermediate parse results
data ParseResult st a = PR { prResult :: a, prState :: State st }

instance Functor (ParseResult st) where
  fmap f pr = pr { prResult = f $ prResult pr }

-- | Continutation passing style ambiguity parser
type Continuation st a b =
  ParseError -> [ParseResult st a] -> [ParseResult st a] -> b
type EmptyFail    b = ParseError -> b
type ConsumedFail b = ParseError -> b


newtype Parser st a = Parser {runParser :: forall b .
     State st
  -> Continuation st a b
  -> ConsumedFail b
  -> EmptyFail b
  -> b }


-- instance declarations: functor, applicative, monad, alternative, moandplus

instance Functor (Parser st) where
  fmap f p = Parser $ \ st ok cerr err ->
    runParser p st (new ok) cerr err
    where
      new ok err eok cok = ok err (map (fmap f) eok) (map (fmap f) cok)

instance Applicative.Applicative (Parser st) where
  pure = return
  (<*>) = ap

instance Monad (Parser st) where
  return x = Parser $ \st ok _ _ ->
      ok (newErrorUnknown (stPosi st)) [PR x st] []

  p >>= f = Parser $ \st ok cerr eerr ->
    let pok = tryParses f ok cerr eerr
        pcerr = cerr
        peerr = eerr
    in runParser p st pok pcerr peerr

  fail s = Parser $ \st _ _ eerr ->
    eerr $ newErrorMessage (newMessage s) (stPosi st)


-- The reverses are just for debugging to force an intuitive order,
-- but are not necessary at all.
-- This function is simple, but unfriendly to read because of all the
-- accumulators involved. A clearer definition would be welcome.
tryParses :: (a -> Parser st b) -> Continuation st b c -> ConsumedFail c
          -> EmptyFail c -> ParseError -> [ParseResult st a]
          -> [ParseResult st a] ->  c
tryParses f ok cerr eerr err eok cok = accumE err [] [] [] [] eok
  where
    accumE acc_err acc_eok acc_cok acc_cerr acc_eerr ((PR a st'):rs) =
      let fok ferr feok fcok =
            accumE (acc_err <++> ferr) (reverse feok ++ acc_eok)
                   (reverse fcok ++ acc_cok) acc_cerr acc_eerr rs
          fcerr err' =
            accumE acc_err acc_eok acc_cok (err':acc_cerr) acc_eerr rs
          feerr err' =
            accumE acc_err acc_eok acc_cok acc_cerr (err':acc_eerr) rs
      in  runParser (f a) st' fok fcerr feerr
    accumE acc_err acc_eok acc_cok acc_cerr acc_eerr [] =
      accumC acc_err acc_eok acc_cok acc_cerr acc_eerr cok

    accumC acc_err acc_eok acc_cok acc_cerr acc_eerr ((PR a st'):rs) =
      let fok ferr feok fcok =
            accumC (acc_err <+> ferr) acc_eok
              (reverse feok ++ reverse fcok ++ acc_cok) acc_cerr acc_eerr rs
          fcerr err' =
            accumC acc_err acc_eok acc_cok (err':acc_cerr) acc_eerr rs
          feerr err' =
            accumC acc_err acc_eok acc_cok (err':acc_cerr) acc_eerr rs
      in  runParser (f a) st' fok fcerr feerr
    accumC acc_err acc_eok acc_cok acc_cerr acc_eerr []
      | (not $ null acc_eok)  || (not $ null acc_cok)  = ok acc_err (reverse acc_eok) (reverse acc_cok)
      | (not $ null acc_eerr) = eerr $ foldl' (<++>) err $ acc_eerr ++ acc_cerr
      | (not $ null acc_cerr) = cerr $ foldl' (<++>) err $ acc_cerr
      | otherwise = error "tryParses: parser has empty result"


instance Applicative.Alternative (Parser st) where
  empty = mzero
  (<|>) = mplus


instance MonadPlus (Parser st) where
  mzero       = Parser $ \st _ _ eerr -> eerr $ newErrorUnknown (stPosi st)
  mplus m n = Parser $ \st ok cerr eerr ->
    let meerr err =
          let nok   err' = ok   $ err <+>  err'
              ncerr err' = cerr $ err <++> err'
              neerr err' = eerr $ err <++> err'
          in  runParser n st nok ncerr neerr
    in  runParser m st ok cerr meerr



-- Escaping the parser

---- Reply type
data Reply a = Ok [a] | Error ParseError

---- running the parser
runP :: Parser st a -> State st -> Reply (ParseResult st a)
runP p st = runParser p st ok cerr eerr
  where
    ok _ eok cok = Ok $ eok ++ cok
    cerr err     = Error err
    eerr err     = Error err


-- parser state management

instance MonadState st (Parser st) where
  get   = Parser $ \st ok _ _ ->
    ok (newErrorUnknown (stPosi st)) [PR (stUser st) st] []
  put s = Parser $ \st ok cerr eerr ->
    ok (newErrorUnknown (stPosi st)) [PR () st {stUser = s}] []


getInput :: Parser st [Token]
getInput = Parser $ \st@(State _ inp) ok _ _ ->
  ok (newErrorUnknown (stPosi st)) [PR inp st] []

getPos :: Parser st SourcePos
getPos = Parser $ \st ok _ _ ->
  ok (newErrorUnknown (stPosi st)) [PR (stPosi st) st] []

getText :: [Token] -> Parser st String
getText inp = Parser $ \st ok _ _ ->
  let pos = stPosi st
      txt = composeToken $ takeWhile ( (>) pos . tokenPos ) inp
  in  ok (newErrorUnknown (stPosi st)) [PR txt st] []
