module Alice.Data.Tag where
{- Tags are extralogical information that we may attach to Formulae -}
data Tag =
  DIG | DMS | DMP | DHD | DIH | DCH | DEC |
  -- Tags to mark certain parts of function definitions
  DMK | DEV | DCD | DEF | DDM | DRP |
  -- Tags to mark parts in function proof tasks
  FDM -- domain
  | FEX -- existence
  | FUQ -- uniqueness
  | FCH -- choice
  | FDC -- condition
  deriving (Eq, Show)

{- whether a Tag marks a part in a function proof task -}
fnTag :: Tag -> Bool
fnTag FDM = True; fnTag FCH = True; fnTag FEX = True; fnTag FUQ = True
fnTag _   = False