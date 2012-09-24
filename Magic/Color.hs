module Magic.Color
   ( CombColor (..)
   , colorless
   , colors

   , Color1 (..)
   , fromColor1

   , Color2 (..)
   , color2
   , splitColor2
   , fromColor2

   ) where

import Prelude
import Data.Maybe

-- CombColor
data CombColor = CombColor
   { isWhite :: Bool
   , isBlue  :: Bool
   , isBlack :: Bool
   , isRed   :: Bool
   , isGreen :: Bool
   }
   deriving (Eq)

colorless :: CombColor
colorless = CombColor False False False False False

colors :: CombColor -> [Color1]
colors c = mapMaybe (($c) . maybeColor) [White ..]

fromColors :: [Color1] -> CombColor
fromColors (White:cs) = (fromColors cs) { isWhite = True }
fromColors (Blue :cs) = (fromColors cs) { isBlue  = True }
fromColors (Black:cs) = (fromColors cs) { isBlack = True }
fromColors (Red  :cs) = (fromColors cs) { isRed   = True }
fromColors (Green:cs) = (fromColors cs) { isGreen = True }

maybeColor :: Color1 -> CombColor -> Maybe Color1
maybeColor White x = if isWhite x then Just White else Nothing
maybeColor Blue  x = if isBlue  x then Just Blue  else Nothing
maybeColor Black x = if isBlack x then Just Black else Nothing
maybeColor Red   x = if isRed   x then Just Red   else Nothing
maybeColor Green x = if isGreen x then Just Green else Nothing

-- Color1
data Color1 = White
           | Blue
           | Black
           | Red
           | Green
           deriving (Eq, Enum)

fromColor1 :: Color1 -> CombColor
fromColor1 White = CombColor True  False False False False
fromColor1 Blue  = CombColor False True  False False False
fromColor1 Black = CombColor False False True  False False
fromColor1 Red   = CombColor False False False True  False
fromColor1 Green = CombColor False False False False True

-- Color2
data Color2 = WU
            | WB
            | UB
            | UR
            | BR
            | BG
            | RG
            | RW
            | GW
            | GU
            deriving (Eq, Enum)

color2 :: Color1 -> Color1 -> Maybe Color2
color2 White Blue  = Just WU
color2 White Black = Just WB
color2 Blue  Black = Just UB
color2 Blue  Red   = Just UR
color2 Black Red   = Just BR
color2 Black Green = Just BG
color2 Red   Green = Just RG
color2 Red   White = Just RW
color2 Green White = Just GW
color2 Green Blue  = Just GU
color2 x     y
   | x == y        = Nothing
   | otherwise     = color2 y x

splitColor2 :: Color2 -> (Color1, Color1)
splitColor2 WU = (White, Blue )
splitColor2 WB = (White, Black)
splitColor2 UB = (Blue , Black)
splitColor2 UR = (Blue , Red  )
splitColor2 BR = (Black, Red  )
splitColor2 BG = (Black, Green)
splitColor2 RG = (Red  , Green)
splitColor2 RW = (Red  , White)
splitColor2 GW = (Green, White)
splitColor2 GU = (Green, Blue )

fromColor2 :: Color2 -> CombColor
fromColor2 WU = fromColors [White, Blue ]
fromColor2 WB = fromColors [White, Black]
fromColor2 UB = fromColors [Blue , Black]
fromColor2 UR = fromColors [Blue , Red  ]
fromColor2 BR = fromColors [Black, Red  ]
fromColor2 BG = fromColors [Black, Green]
fromColor2 RG = fromColors [Red  , Green]
fromColor2 RW = fromColors [Red  , White]
fromColor2 GW = fromColors [Green, White]
fromColor2 GU = fromColors [Green, Blue ]

-- Show isntances

-- vim: set expandtab:
