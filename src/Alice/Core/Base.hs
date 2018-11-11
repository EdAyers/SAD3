{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Verifier state monad and common functions.
-}

{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}


module Alice.Core.Base (
  RState (..), RM,
  askRS, updateRS,
  justIO,
  (<|>),
  runRM,

  GState (..), GM,
  askGlobalState, updateGlobalState,
  addGlobalContext, insertMRule,
  retrieveContext,
  initialGlobalState,
  defForm, getDef, getLink, addGroup,

  VState (..), VM,

  Counter (..), TimeCounter (..), IntCounter (..),
  accumulateIntCounter, accumulateTimeCounter, maximalTimeCounter,
  showTimeDiff,
  timer,

  askInstructionInt, askInstructionBin, askInstructionString,
  addInstruction, dropInstruction,
  addTimeCounter, addIntCounter, incrementIntCounter,
  guardInstruction, guardNotInstruction, whenInstruction,

  reasonLog, simpLog, thesisLog,
) where

import Control.Monad
import Data.IORef
import Data.List
import Data.Time
import qualified Control.Applicative as App
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader

import Alice.Data.Formula
import Alice.Data.Instr
import Alice.Data.Text.Block (Block, Text)
import Alice.Data.Text.Context (Context, MRule(..))
import qualified Alice.Data.Text.Context as Context (name)
import Alice.Data.Definition (Definitions, DefEntry(DE), DefType(..))
import qualified Alice.Data.Definition as Definition
import Alice.Data.Rules (Rule)
import Alice.Data.Evaluation (Evaluation)
import Alice.Export.Base
import qualified Alice.Data.Structures.DisTree as DT
import Alice.Core.Position
import qualified Alice.Core.Message as Message

import Debug.Trace

-- | Reasoner state
data RState = RState {
  instructions :: [Instr], -- ^ Instruction stack. A stack of all of the instructions so far. Popped when an `iDrop` of the right type is encountered.
  counters     :: [Counter], -- ^ Statistics about what has been done so far.
  provers      :: [Prover] }


{-| The global proof state.-}
data GState = GL {
  -- | All definitions so far
  definitions      :: Definitions,
  -- | positive MESON rules for ontological checking
  mesonPositives   :: DT.DisTree MRule,
  -- | negative MESON rules for ontological checking
  mesonNegatives   :: DT.DisTree MRule,
  -- | groupings of theorem identifiers
  identifierGroups :: M.Map String (Set.Set String),
  -- | the global context
  globalContext    :: [Context],
  -- | a counter for the identifier for skolem constants
  skolemCounter    :: Int 
  }



-- All of these counters are for gathering statistics to print out later

data Counter  =
    TimeCounter TimeCounter NominalDiffTime
  | IntCounter IntCounter Int

data TimeCounter  =
    ProofTime
  | SuccessTime  -- succesful prove time
  | SimplifyTime
  deriving Eq

data IntCounter  =
    Sections
  | Goals
  | FailedGoals
  | TrivialGoals
  | SuccessfulGoals
  | Symbols
  | TrivialChecks
  | HardChecks
  | SuccessfulChecks
  | Unfolds
  | Equations
  | FailedEquations
  deriving Eq


-- CPS IO Maybe monad

type CRMC a b = IORef RState -> IO a -> (b -> IO a) -> IO a
newtype CRM b = CRM { runCRM :: forall a . CRMC a b }

instance Functor CRM where
  fmap = liftM

instance App.Applicative CRM where
  pure = return
  (<*>) = ap

instance Monad CRM where
  return r  = CRM $ \ _ _ k -> k r
  m >>= n   = CRM $ \ s z k -> runCRM m s z (\ r -> runCRM (n r) s z k)

instance App.Alternative CRM where
  empty = mzero
  (<|>) = mplus

instance MonadPlus CRM where
  mzero     = CRM $ \ _ z _ -> z
  mplus m n = CRM $ \ s z k -> runCRM m s (runCRM n s z k) k





type RM = CRM
runRM :: RM a -> IORef RState -> IO (Maybe a)
runRM m s = runCRM m s (return Nothing) (return . Just)

infixl 0 <|>
{-# INLINE (<|>) #-}
(<|>) :: (MonadPlus m) => m a -> m a -> m a
(<|>) = mplus

-- | Verification State.
data VState = VS {
  thesisMotivated :: Bool,
  rewriteRules    :: [Rule],
  evaluations     :: DT.DisTree Evaluation, -- ^ (low level) evaluation rules
  currentThesis   :: Context,
  currentBranch   :: [Block],         -- ^ branch of the current block
  currentContext  :: [Context],
  restText        :: [Text] }



type GM = StateT GState CRM
type VM = ReaderT VState GM

justRS :: VM (IORef RState)
justRS = lift $ lift $ CRM $ \ s _ k -> k s


justIO :: IO a -> VM a
justIO m = lift $ lift $ CRM $ \ _ _ k -> m >>= k

-- State management from inside the verification monad

askRS :: (RState -> b) -> ReaderT VState GM b
askRS f     = justRS >>= (justIO . fmap f . readIORef)
updateRS :: (RState -> RState) -> ReaderT VState GM ()
updateRS f  = justRS >>= (justIO . flip modifyIORef f)

askInstructionInt :: InInt -> Int -> ReaderT VState GM Int
askInstructionInt    instr _default =
  fmap (askII instr _default) (askRS instructions)
askInstructionBin :: InBin -> Bool -> ReaderT VState GM Bool
askInstructionBin    instr _default =
  fmap (askIB instr _default) (askRS instructions)
askInstructionString :: InStr -> String -> ReaderT VState GM String
askInstructionString instr _default =
  fmap (askIS instr _default) (askRS instructions)

addInstruction :: Instr -> ReaderT VState GM ()
addInstruction  instr =
  updateRS $ \rs -> rs { instructions = instr : instructions rs }
dropInstruction instr =
  updateRS $ \rs -> rs { instructions = dropI instr $ instructions rs }
addTimeCounter counter time =
  updateRS $ \rs -> rs { counters = TimeCounter counter time : counters rs }
addIntCounter  counter time =
  updateRS $ \rs -> rs { counters = IntCounter  counter time : counters rs }
incrementIntCounter counter = addIntCounter counter 1

-- time proof tasks
timer counter task = do
  begin  <- justIO $ getCurrentTime
  result <- task
  end    <- justIO $ getCurrentTime
  addTimeCounter counter $ diffUTCTime end begin
  return result

guardInstruction instr _default =
  askInstructionBin instr _default >>= guard
guardNotInstruction instr _default =
  askInstructionBin instr _default >>= guard . not
whenInstruction instr _default action =
  askInstructionBin instr _default >>= \ b -> when b action

-- Counter management

fetchIntCounter  counterList counter =
  [ value | IntCounter  kind value <- counterList, counter == kind ]
fetchTimeCounter counterList counter =
  [ value | TimeCounter kind value <- counterList, counter == kind ]

accumulateIntCounter  counterList startValue =
  foldr (+) startValue . fetchIntCounter counterList
accumulateTimeCounter counterList startValue =
  foldr addUTCTime startValue . fetchTimeCounter counterList
maximalTimeCounter counterList = foldr max 0 . fetchTimeCounter counterList

showTimeDiff t
  | hours == 0 =
      format minutes ++ ':' : format restSeconds ++ '.' : format restCentis
  | True    =
      format hours   ++ ':' : format restMinutes ++ ':' : format restSeconds
  where
    format n = if n < 10 then '0':show n else show n
    centiseconds = truncate $ t * 100
    (seconds, restCentis)  = divMod centiseconds 100
    (minutes, restSeconds) = divMod seconds 60
    (hours  , restMinutes) = divMod minutes 60


-- common messages

reasonLog :: Message.Kind -> SourcePos -> String -> VM ()
reasonLog kind pos = justIO . Message.outputReason kind pos

thesisLog :: Message.Kind -> SourcePos -> Int -> String -> VM ()
thesisLog kind pos indent = justIO . Message.outputThesis kind pos indent

simpLog :: Message.Kind -> SourcePos -> String -> VM ()
simpLog kind pos = justIO . Message.outputSimp kind pos



-- GlobalState management

askGlobalState    = lift . gets
updateGlobalState = lift . modify

insertMRule :: [MRule] -> VM ()
insertMRule rules = updateGlobalState (add rules)
  where
    add [] globalState = globalState
    add (rule@(MR _ conclusion):rules) gs
      | isNot conclusion =
          let negatives     = mesonNegatives gs
              negConclusion = ltNeg conclusion
          in  add rules $
                gs {mesonNegatives = DT.insert negConclusion rule negatives}
      | otherwise  =
          let positives = mesonPositives gs
          in  add rules $
                gs {mesonPositives = DT.insert conclusion rule positives}

initialGlobalState = GL initialDefinitions DT.empty DT.empty M.empty [] 0

addGlobalContext :: Context -> ReaderT VState GM ()
addGlobalContext cn =
  updateGlobalState (\st -> st {globalContext = cn : globalContext st})



retrieveContext :: Set.Set String -> ReaderT VState GM [Context]
retrieveContext names = do
  globalContext <- askGlobalState globalContext
  let (context, unfoundSections) = runState (retrieve globalContext) names
  -- warn a user if some sections could not be found
  unless (Set.null unfoundSections) $ warn unfoundSections
  return context
  where
    warn unfoundSections =
      reasonLog Message.WARNING noPos $
        "Could not find sections " ++ unwords (map show $ Set.elems unfoundSections)
    retrieve [] = return []
    retrieve (context:restContext) = let name = Context.name context in
      gets (Set.member name) >>= \p -> if p
      then modify (Set.delete name) >> fmap (context:) (retrieve restContext)
      else retrieve restContext




-- Definition management

-- initial definitions
initialDefinitions = IM.fromList [
  (-1,  equality),
  (-2,  less),
  (-4,  function),
  (-5,  functionApplication),
  (-6,  domain),
  (-7,  set),
  (-8,  elementOf),
  (-10, pair) ]

equality  = DE [] Top Signature (zEqu (zVar "?0") (zVar "?1")) [] []
less      = DE [] Top Signature (zLess (zVar "?0") (zVar "?1")) [] []
set       = DE [] Top Signature (zSet $ zVar "?0") [] []
elementOf = DE [zSet $ zVar "?1"] Top Signature
  (zElem (zVar "?0") (zVar "?1")) [] [[zSet $ zVar "?1"]]
function  = DE [] Top Signature (zFun $ zVar "?0") [] []
domain    = DE [zFun $ zVar "?0"] (zSet ThisT) Signature
  (zDom $ zVar "?0") [zSet ThisT] [[zFun $ zVar "?0"]]
pair      = DE [] Top Signature (zPair (zVar "?0") (zVar "?1")) [] []
functionApplication =
  DE [zFun $ zVar "?0", zElem (zVar $ "?1") $ zDom $ zVar "?0"] Top Signature
    (zApp (zVar "?0") (zVar "?1")) []
    [[zFun $ zVar "?0"],[zElem (zVar $ "?1") $ zDom $ zVar "?0"]]


-- | retrieve definitional formula of a term
defForm :: IM.IntMap DefEntry -> Formula -> Maybe Formula
defForm definitions term = do
  def <- IM.lookup (trId term) definitions
  guard $ Definition.isDefinition def
  sb <- match (Definition.term def) term
  return $ sb $ Definition.formula def


-- | retrieve definition of a symbol (monadic)
getDef :: Formula -> VM DefEntry
getDef term = do
  defs <- askGlobalState definitions
  let mbDef = IM.lookup (trId term) defs
  guard $ isJust mbDef
  return $ fromJust mbDef

-- groupings

-- | get the section identifiers grouped by a group identifier
getLink :: [String] -> VM (Set.Set String)
getLink link = do
  groups <- askGlobalState identifierGroups
  return $ Set.unions $
    map (\l -> M.findWithDefault (Set.singleton l) l groups) link

-- | add group identifier
addGroup :: [String] -> VM ()
addGroup [] = return ()
addGroup [name] = reasonLog Message.WARNING noPos $ "empty group: " ++ show name
addGroup (name:identifiers) =
  getLink identifiers >>= \link -> updateGlobalState
    (\st -> st {identifierGroups = M.insert name link $ identifierGroups st})
