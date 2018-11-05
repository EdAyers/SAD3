module Alice.Data.Text.Block (
  Text(..),
  Block(..),
  makeBlock,
  Section(..),
  showForm,
  formulate,
  compose,
  needsProof,
  isTopLevel,
  file
  )where

import Alice.Data.Formula
import Alice.Core.Position
import Alice.Data.Instr (Instr, Idrop)

{-| Text is either a `Block` of ForTheL text or an instruction. -}
data Text = TB Block | TI Instr | TD Idrop

{-| A block is a chunk of text. For example a `Theorem` or `Signature` block.
-}
data Block  = Block {
  -- logical fields.
  formula           :: Formula, -- ^ Formula image, see Andrei's thesis.
  body              :: [Text], -- ^ The proof body.
  kind              :: Section, -- ^ The `Definition` in `Definition foo.`.
  declaredVariables :: [String], -- ^ Used during parsing. Assumptions can declare variables and Choice can declare variables.
  -- extra fields for user communication.
  name              :: String,  -- ^ The user provided name for the Block. Eg the foo in `Definition foo.`
  link              :: [String], -- ^ The definitions referenced in `(by X, Y)`.
  position          :: SourcePos, -- ^ position in the sourcetext file.
  text              :: String } -- ^ the sourcetext that was parsed to this block. Modulo whitespace.

  
makeBlock :: Formula -> [Text] -> Section -> String -> [String] -> SourcePos -> String -> Block
makeBlock form body kind name link pos txt =
  Block form body kind [] name link (rangePos (pos, advancesPos pos txt)) txt

{-| All possible types that a ForThel block can have. -}
data Section =
  Definition | Signature | Axiom       | Theorem | CaseHypothesis  |
  Assumption | Selection | Affirmation | Posit   | LowDefinition
  deriving Eq

-- Composition

{- form the formula image of a whole block -}
formulate :: Block -> Formula
formulate block
  | isTopLevel block = compose $ body block
  | otherwise = formula block

compose :: [Text] -> Formula
compose = foldr comp Top
  where
    comp (TB block@Block{ declaredVariables = dvs }) f
      | needsProof block || kind block == Posit =
          foldr zExi (blAnd (formulate block) f) dvs
      | otherwise = foldr zAll (blImp (formulate block) f) dvs
    comp _ fb = fb



{- necessity of proof as derived from the block type -}
needsProof :: Block -> Bool
needsProof block = sign $ kind block
  where
    sign Definition = False
    sign Signature  = False
    sign Axiom      = False
    sign Assumption = False
    sign Posit      = False
    sign _          = True


isTopLevel :: Block -> Bool
isTopLevel  = isHole . formula

noBody :: Block -> Bool
noBody  = null . body

file :: Block -> String
file = sourceFile . position

-- Show instances

instance Show Text where
  showsPrec p (TB block) = showsPrec p block
  showsPrec 0 (TI instruction) = shows instruction . showChar '\n'
  showsPrec 0 (TD instruction) = shows instruction . showChar '\n'
  showsPrec _ _ = id

instance Show Block where
  showsPrec p block@Block {body = body}
    | noBody block = showForm p block
    | isTopLevel block = showForm p block . showBody
    | otherwise = showForm p block .
        showIndent p . showString "proof.\n" . showBody .
        showIndent p . showString "qed.\n"
    where
      showBody = foldr ((.) . showsPrec (succ p)) id body

showForm p block@Block {formula = formula, name = name} =
  showIndent p . sform (isTopLevel block) (needsProof block) . dot
  where
    sform True  True  = showString $ "conjecture" ++ addName
    sform True  False = showString $ "hypothesis" ++ addName
    sform False False = showString "assume " . shows formula
    sform False True  = shows formula

    addName = if null name then "" else (' ':name)
    dot = showString ".\n"

showIndent :: Int -> ShowS
showIndent n = showString $ replicate (n * 2) ' '