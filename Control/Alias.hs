module AbLib.Control.Alias where

-- "then" operator. Composes functions left-to-right
($>) = flip ($)
infixl 0 $>