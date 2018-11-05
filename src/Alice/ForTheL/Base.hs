{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

FoTheL state and state management, parsing of primitives, operations on variables and macro expressions.
-}



module Alice.ForTheL.Base where

import Control.Monad
import qualified Control.Monad.State.Class as MS
import Data.Char
import Data.List

import Alice.Data.Formula

import Alice.Parser.Base
import Alice.Parser.Combinators
import Alice.Parser.Primitives

import Debug.Trace
import Alice.Parser.Token

-- | A parser of ForTheL text.
type FTL = Parser FState

-- |unary term
type UTerm    = (Formula -> Formula, Formula)
-- |unary notion
type UNotion  = (Formula -> Formula, Formula, String)
-- |multi-argument term
type MTerm    = (Formula -> Formula, [Formula])
-- |multi-argument notion.
type MNotion  = (Formula -> Formula, Formula, [String])

-- | A `Prim` is a pattern and a function that turns the resulting values of that pattern into a `Formula`
type Prim     = ([Patt], [Formula] -> Formula)

{- 
-}

{-| The state of the ForTheL parser. 
It contains all of the currently defined patterns and context. 

Considering `cfn_expr, rfn_expr, lfn_expr, ifn_expr, cpr_expr, rpr_expr, lpr_expr, ipr_expr`:
there are four kinds of pattern based on whether they begin with a token or a variable.
- `c` or "closed" `T .. T`
- `l` or "postfix" or "left" `v .. T`
- `r` or "prefix" or "right" `T .. v`
- `i` or "infix" `v .. v`
These different cases allow parsing of symbolic terms while respecting precedence and so on.
-}
data FState = FState {
  -- | Adjectives
  adj_expr :: [Prim],
  -- | Verbs
  ver_expr :: [Prim],
  -- | Notions
  ntn_expr :: [Prim],
  -- | Symbolic Notions?? [FIXME]
  snt_expr :: [Prim],
  -- | Patterns for terms. 
  cfn_expr, rfn_expr, lfn_expr, ifn_expr :: [Prim],
  -- | Patterns for predicates.
  cpr_expr, rpr_expr, lpr_expr, ipr_expr :: [Prim],

  -- | Pretyped variables. For example you might define `Let G stand for a group.` earlier in the file and you want to use it later.
  tvr_expr :: [TVar], 
  -- | String synonyms. Created by `[set/-s]` notation.
  str_syms :: [[String]], 
  -- | Variables that are in scope. What is the difference between this and `tvr_expr`? [FIXME]
  var_decl :: [String], 
  -- | [TODO] docstring
  id_count :: Int, 
  -- | [TODO] docstring
  in_decl  :: Bool, 
  -- | The number of hidden variables in scope. [FIXME] these are things like `For all X there is a Y`. There is implicitly a variable involved.
  hid_count :: Int 
  }

{-| Initial state. Contains some basic notions and patterns. -}
initFS :: FState
initFS  = FState  eq [] nt sn
                  cf rf [] []
                  [] [] [] sp
                  [] [] [] 0 False 0
  where
    eq  = [ ([Wd ["equal"], Wd ["to"], Vr], zTrm (-1) "="),
            ([Wd ["nonequal"], Wd ["to"], Vr], Not . zTrm (-1) "=") ]
    sp  = [ ([Sm "="], zTrm (-1) "="),
            ([Sm "!", Sm "="], Not . zTrm (-1) "="),
            ([Sm "-", Sm "<", Sm "-"], zTrm (-2) "iLess"),
            ([Sm "-~-"], \(m:n:_) -> zAll "" $ Iff (zElem (zVar "") m) (zElem (zVar "") n)) ]
    sn  = [ ([Sm "=", Vr], zTrm (-1) "=") ]
    nt  = [ ([Wd ["function","functions"], Nm], zFun . head),
            ([Wd ["set","sets"], Nm], zSet . head),
            ([Wd ["element", "elements"], Nm, Wd ["of"], Vr], \(x:m:_) -> zElem x m)]
    rf  = [ ([Sm "[", Vr, Sm "]"], \(f:x:_) -> zApp f x)]
    cf  = [ ([Sm "Dom", Sm "(",Vr,Sm ")"], zDom . head),
            ([Sm "(", Vr, Sm ",", Vr, Sm ")"], \(x:y:_) -> zPair x y) ]

            
getExpr :: (FState -> [a]) -> (a -> FTL b) -> FTL b
getExpr e p = MS.gets e >>= tryAll . map (unexpectedUnit . try . p)

getDecl :: FTL [String]
getDecl = MS.gets var_decl

addDecl :: [String] -> FTL a -> FTL a
addDecl vs p  = do
  dcl <- MS.gets var_decl; MS.modify adv;
  after p $ MS.modify $ sbv dcl
  where
    adv s     = s { var_decl = vs ++ var_decl s }
    sbv vs s  = s { var_decl = vs }

getPretyped :: FTL [TVar]
getPretyped = MS.gets tvr_expr

-- Predicates: verbs and adjectives

prim_ver, prim_adj, prim_un_adj :: FTL UTerm -> FTL UTerm

prim_ver      = getExpr ver_expr . prim_prd
prim_adj      = getExpr adj_expr . prim_prd
prim_un_adj   = getExpr (filter (unary . fst) . adj_expr) . prim_prd
  where
    unary pt  = Vr `notElem` pt

prim_prd :: FTL UTerm -> Prim -> FTL UTerm
prim_prd p (pt, fm) = do  (q, ts) <- wd_patt p pt
                          return (q, fm $ zHole:ts)


-- | Multi-subject predicates: [a,b are] equal
prim_m_ver, prim_m_adj, prim_m_un_adj :: FTL UTerm -> FTL UTerm
-- | Multi-subject Verb. eg `x dominates y` [FIXME]
prim_m_ver    = getExpr ver_expr . prim_ml_prd
-- | Multi-subject Adjective. eg `x, y are parallel`.
prim_m_adj    = getExpr adj_expr . prim_ml_prd
-- | Multi-subject unary adjective. eg `x, y are even`. [FIXME] is this right?
prim_m_un_adj = getExpr (filter (unary . fst) . adj_expr) . prim_ml_prd
  where
    unary (Vr : pt) = Vr `notElem` pt
    unary (_  : pt) = unary pt
    unary _         = True

prim_ml_prd :: FTL UTerm -> Prim -> FTL UTerm
prim_ml_prd p (pt, fm)  = do  (q, ts) <- ml_patt p pt
                              return (q, fm $ zHole:zSlot:ts)


-- Notions and functions

prim_ntn, prim_of_ntn :: FTL UTerm -> FTL MNotion

prim_ntn p  = getExpr ntn_expr ntn
  where
    ntn (pt, fm)  = do  (q, vs, ts) <- nt_patt p pt
                        return (q, fm $ zHole:ts, vs)

prim_of_ntn p = getExpr ntn_expr ntn
  where
    ntn (pt, fm)  = do  (q, vs, ts) <- of_patt p pt
                        let fn v = fm $ (zVar v):zHole:ts
                        return (q, foldr1 And $ map fn vs, vs)

prim_cm_ntn :: FTL UTerm -> FTL MTerm -> FTL MNotion
prim_cm_ntn p s = getExpr ntn_expr ntn
  where
    ntn (pt, fm)  = do  (q, vs, as, ts) <- cm_patt p s pt
                        let fn v = fm $ zHole:v:ts
                        return (q, foldr1 And $ map fn as, vs)

prim_fun :: FTL UTerm -> FTL UTerm
prim_fun  = (>>= fun) . prim_ntn
  where
    fun (q, Trm {trName = "=", trArgs = [_, t]}, _) | not (occursH t) = return (q, t)
    fun _ = mzero


-- Symbolic primitives

prim_cpr :: FTL Formula -> FTL Formula
prim_cpr  = getExpr cpr_expr . prim_csm     -- T ... T
prim_rpr :: FTL Formula -> FTL (Formula -> Formula)
prim_rpr  = getExpr rpr_expr . prim_rsm     -- v ... T
prim_lpr :: FTL Formula -> FTL (Formula -> Formula)
prim_lpr  = getExpr lpr_expr . prim_lsm     -- T ... v
prim_ipr :: FTL Formula -> FTL (Formula -> Formula -> Formula)
prim_ipr  = getExpr ipr_expr . prim_ism     -- v ... v

prim_cfn :: FTL Formula -> FTL Formula
prim_cfn  = getExpr cfn_expr . prim_csm
prim_rfn :: FTL Formula -> FTL (Formula -> Formula)
prim_rfn  = getExpr rfn_expr . prim_rsm
prim_lfn :: FTL Formula -> FTL (Formula -> Formula)
prim_lfn  = getExpr lfn_expr . prim_lsm
prim_ifn :: FTL Formula -> FTL (Formula -> Formula -> Formula)
prim_ifn  = getExpr ifn_expr . prim_ism

prim_csm :: FTL Formula -> Prim -> FTL Formula
prim_csm p (pt, fm) = sm_patt p pt >>= \l -> return $ fm l
prim_rsm :: FTL Formula -> Prim -> FTL (Formula -> Formula)
prim_rsm p (pt, fm) = sm_patt p pt >>= \l -> return $ \t -> fm $ t:l
prim_lsm :: FTL Formula -> Prim -> FTL (Formula -> Formula)
prim_lsm p (pt, fm) = sm_patt p pt >>= \l -> return $ \s -> fm $ l++[s]
prim_ism :: FTL Formula -> Prim -> FTL (Formula -> Formula -> Formula)
prim_ism p (pt, fm) = sm_patt p pt >>= \l -> return $ \t s -> fm $ t:l++[s]
prim_snt :: FTL Formula -> FTL MNotion
prim_snt p  = noError $ varlist >>= getExpr snt_expr . snt
  where
    snt vs (pt, fm) = sm_patt p pt >>= \l -> return (id, fm $ zHole:l, vs)



{-|A __pattern__ is a list of `Patt`s and represents a parsing instruction for user-defined syntax. 
You can see some examples of patterns in `initFS`.
-}
data Patt = 
  -- |expecting a word or collection of words
  Wd [String]
  -- |expecting a symbol
  | Sm String  
  -- |expecting a term variable
  | Vr  
  -- |expecting a name for a new variable. 
  | Nm  
  deriving (Eq, Show)

-- | Check if two patterns are equal.
samePat :: [Patt] -> [Patt] -> Bool
samePat [] [] = True
samePat ((Wd ls) : rst1) ((Wd rs) : rst2) = all (`elem` rs) ls && samePat rst1 rst2
samePat (Vr : rst1) (Vr : rst2) = samePat rst1 rst2
samePat (Nm : rst1) (Nm : rst2) = samePat rst1 rst2
samePat ((Sm s) : rst1) ((Sm t) : rst2) = s == t && samePat rst1 rst2
samePat _ _ = False

-- | Most basic pattern parser: simply follow the pattern any parse terms with `p`
-- at variable places
wd_patt :: FTL UTerm -> [Patt] -> FTL MTerm
wd_patt p (Wd l : ls) = wd_tokenOf l >> wd_patt p ls
wd_patt p (Vr : ls)   = do  (r, t) <- p
                            (q, ts) <- wd_patt p ls
                            return (r . q, t:ts)
wd_patt _ []          = return (id, [])
wd_patt _ _           = mzero

-- | parses a symbolic pattern
sm_patt :: FTL Formula -> [Patt] -> FTL [Formula]
sm_patt p (Vr : ls)   = liftM2 (:) p $ sm_patt p ls
sm_patt p (Sm s : ls) = sm_token s >> sm_patt p ls
sm_patt _ []          = return []
sm_patt _ _           = mzero

-- |parses a multi-subject pattern: follow the pattern, but ignore the wd_token
-- right before the first variable. Then check that all "and" tokens have been
-- consumed. Example pattern: `[Wd ["commute","commutes"], Wd ["with"], Vr]`. Then we can parse
-- "a commutes with c and d" as well as "a and b commute".
ml_patt :: FTL UTerm -> [Patt] -> FTL MTerm
ml_patt p (Wd l :_: Vr : ls)
                      = wd_tokenOf l >> na_patt p ls
ml_patt p (Wd l : ls) = wd_tokenOf l >> ml_patt p ls
ml_patt _ _           = mzero


-- |parses a notion: follow the pattern to the name place, record names,
-- |then keep following the pattern
nt_patt :: FTL UTerm -> [Patt] -> FTL (Formula -> Formula, [String], [Formula])
nt_patt p (Wd l : ls) = wd_tokenOf l >> nt_patt p ls
nt_patt p (Nm : ls)   = do  vs <- namlist
                            (q, ts) <- wd_patt p ls
                            return (q, vs, ts)
nt_patt _ _           = mzero

-- |parse an "of"-notion: follow the pattern to the notion name, then check that
-- "of" follows the name followed by a variable that is not followed by "and"
of_patt :: FTL UTerm -> [Patt] -> FTL (Formula -> Formula, [String], [Formula])
of_patt p (Wd l : ls) = wd_tokenOf l >> of_patt p ls
of_patt p (Nm : Wd l : Vr : ls)
                      = do  guard $ elem "of" l; vs <- namlist
                            (q, ts) <- na_patt p ls
                            return (q, vs, ts)
of_patt _ _           = mzero

-- |parse a "common"-notion: basically like the above. We use the special parser
-- s for the first variable place after the "of" since we expect multiple terms
-- here. Example: A common *divisor of m and n*.
cm_patt :: FTL UTerm -> FTL MTerm -> [Patt] -> FTL (Formula -> Formula, [String], [Formula], [Formula])
cm_patt p s (Wd l:ls) = wd_tokenOf l >> cm_patt p s ls
cm_patt p s (Nm : Wd l : Vr : ls)
                      = do  guard $ elem "of" l; vs <- namlist; wd_tokenOf l
                            (r, as) <- s; when (null $ tail as) $
                              fail "several objects expected for `common'"
                            (q, ts) <- na_patt p ls
                            return (r . q, vs, as, ts)
cm_patt _ _ _         = mzero

-- |an auxiliary pattern parser that checks that we are not dealing wiht an "and"
-- `wd_token` and then continues to follow the pattern
na_patt :: FTL UTerm -> [Patt] -> FTL MTerm
na_patt p (Wd l : ls) = guard (not $ elem "and" l) >> wd_tokenOf l >> wd_patt p ls
na_patt p ls          = wd_patt p ls



-- Variables

namlist :: FTL [String]
namlist = varlist -|- liftM (:[]) hidden

-- | Parses a list of variable names separated by commas.
varlist :: FTL [String]
varlist = do  vs <- var `sepBy` wd_token ","
              nodups vs ; return vs

nodups :: Monad f => [String] -> f ()
nodups vs = unless ((null :: [b] -> Bool) $ duplicateNames vs) $
              fail $ "duplicate names: " ++ show vs

-- | A __hidden__ variable is a variable which 
hidden :: FTL String
hidden  = do n <- MS.gets hid_count
             MS.modify $ \st -> st {hid_count = succ n}
             return ('h':show n)

-- | Parse a variable name.
var :: FTL String
var     = do v <- satisfy (\s -> all isAlphaNum s && isAlpha (head s))
             return ('x':v)

--- pretyped Variables

-- | A typed variable. [FIXME] what exactly does 'typed' mean?
type TVar = ([String], Formula)

prim_tvr :: FTL MNotion
prim_tvr  = getExpr tvr_expr tvr
  where
    tvr (vr, nt)  = do  vs <- varlist
                        guard $ all (`elem` vr) vs
                        return (id, nt, vs)

-- free

freeVars :: Formula -> FTL [String]
freeVars f = do dvs <- getDecl; return $ free dvs f

--- decl

decl :: [String] -> Formula -> [String]
decl vs = dive
  where
    dive (All _ f)  = dive f
    dive (Exi _ f)  = dive f
    dive (Tag _ f)  = dive f
    dive (Imp f g)  = filter (noc f) (dive g)
    dive (And f g)  = dive f `union` filter (noc f) (dive g)
    dive Trm {trName = 'a':_, trArgs = v@Var{trName = u@('x':_)}:ts}
      | all (not . occurs v) ts = guardNotElem vs u
    dive Trm{trName = "=", trArgs = [v@Var{trName = u@('x':_)}, t]}
      | isTrm t && not (occurs v t) = guardNotElem vs u
    dive _  = []

    noc f v = not $ occurs (zVar v) f


overfree :: [String] -> Formula -> Maybe String
overfree vs f
    | occurs zSlot f  = Just $ "too few subjects for an m-predicate " ++ inf
    | not (null sbs)  = Just $ "free undeclared variables: "   ++ sbs ++ inf
    | not (null ovl)  = Just $ "overlapped variables: "        ++ ovl ++ inf
    | otherwise       = Nothing
  where
    sbs = unwords $ map showVar $ free vs f
    ovl = unwords $ map showVar $ over vs f
    inf = "\n in translation: " ++ show f

    over vs (All v f) = bvrs vs v f
    over vs (Exi v f) = bvrs vs v f
    over vs f = foldF (over vs) f

    bvrs vs v f | elem v vs = [v]
                | null v    = over vs f
                | otherwise = over (v:vs) f


--- macro expressions

comma, is, art, an, the, iff, that, standFor, arrow, there, does, has, with, such :: FTL ()

comma = wd_tokenOf [",", "and"]
is    = wd_tokenOf ["is", "be", "are"]
{-| `art` ::= "" | "a" | "an" | "the" -}
art = opt () $ wd_tokenOf ["a","an","the"]
an = wd_tokenOf ["a", "an"]
the = wd_token "the"
iff = wd_token "iff" <|> mapM_ wd_token ["if", "and", "only", "if"]
that  = wd_token "that"
standFor = wd_token "denote" <|> (wd_token "stand" >> wd_token "for")
arrow = symbol "->"
there = wd_token "there" >> wd_tokenOf ["is","exist","exists"]
does = opt () $ wd_tokenOf ["does", "do"]
has = wd_tokenOf ["has" , "have"]
with  = wd_tokenOf ["with", "of", "having"]
such  = wd_tokenOf ["such", "so"]


--just for now:

showVar :: String -> String
showVar ('x':nm) = nm
showVar nm = nm
