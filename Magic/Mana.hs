module Magic.Mana
   ( ManaSymbol (..)
   , manaSymbolHasColor

   , ManaCost (..)
   , manaCostHasColor
   , manaCostCombColor
   ) where

import Magic.Color
import Prelude

-- ManaSymbol
data ManaSymbol = GenericManaSymbol Int
                | GenericManaSymbolX
                | ColorManaSymbol Color1
                | HybridManaSymbol Color2
                | MonocoloredHybridManaSymbol Color1
                | PhyrexianManaSymbol Color1
                deriving (Eq)

instance Show ManaSymbol where
   show (GenericManaSymbol n)           = "(" ++ show n ++ ")"
   show GenericManaSymbolX              = "(X)"
   show (ColorManaSymbol c)             = "(" ++ [colorChar c] ++ ")"
   show (HybridManaSymbol c)            = "(" ++ [colorChar c1] ++ "/" ++ [colorChar c2] ++ ")"
      where (c1, c2) = splitColor2 c
   show (MonocoloredHybridManaSymbol c) = "(2/" ++ [colorChar c] ++ ")"
   show (PhyrexianManaSymbol c)         = "(" ++ [colorChar c] ++ "/P)"

manaSymbolHasColor :: Color1 -> ManaSymbol -> Bool
manaSymbolHasColor _  (GenericManaSymbol _)            = False
manaSymbolHasColor _  GenericManaSymbolX               = False
manaSymbolHasColor c1 (ColorManaSymbol c2)             = c1 == c2
manaSymbolHasColor c1 (HybridManaSymbol c2)            = c1 == c3 || c1 == c4
   where
      (c3, c4) = splitColor2 c2
manaSymbolHasColor c1 (MonocoloredHybridManaSymbol c2) = c1 == c2
manaSymbolHasColor c1 (PhyrexianManaSymbol c2)         = c1 == c2

-- ManaCost
newtype ManaCost = ManaCost { manaCost :: [ManaSymbol] }
                 deriving (Eq)

instance Show ManaCost where
   show = concat . map show . manaCost

manaCostHasColor :: Color1 -> ManaCost -> Bool
manaCostHasColor c mc = any (manaSymbolHasColor c) (manaCost mc)

manaCostCombColor :: ManaCost -> CombColor
manaCostCombColor mc = CombColor
   { isWhite = manaCostHasColor White mc
   , isBlue  = manaCostHasColor Blue  mc
   , isBlack = manaCostHasColor Black mc
   , isRed   = manaCostHasColor Red   mc
   , isGreen = manaCostHasColor Green mc
   }

-- Color to Char
colorChar :: Color1 -> Char
colorChar White = 'W'
colorChar Blue  = 'U'
colorChar Black = 'B'
colorChar Red   = 'R'
colorChar Green = 'G'

-- vim: set expandtab:
