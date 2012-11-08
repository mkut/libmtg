module Magic.Card
   ( Card (..)

   , CardSet (..)
   , CardType (..)
   , Supertype (..)
   , Subtype (..)
   , Rarity (..)
   , Power (..)
   , Toughness (..)

   , withRarity
   , withCardType
   , withSupertype
   , withSubtype

   , toSubtype

   , cardColors
   , cardSet

   , showTypes
   , showPT

   , RealCard (..)
   , foil
   , nonFoil

   ) where

import Magic.Color
import Magic.Mana
import Prelude
import Data.List

data CardSet = CardSet
   { cardSetName :: String
   } deriving (Eq, Ord, Show)

data CardType = Land
              | Creature
              | Enchantment
              | Artifact
              | Instant
              | Sorcery
              | Tribal
              | Planeswalker
              deriving (Eq, Show, Read)

data Supertype = Basic
               | Legendary
               | World
               | Snow
               | Ongoing
               deriving (Eq, Show, Read)

data Subtype = CreatureType String
             | LandType String
             | SpellType String
             | ArtifactType String
             | EnchantmentType String
             | PlaneswalkerType String
             deriving (Eq)

instance Show Subtype where
   show (CreatureType t)     = t
   show (LandType t)         = t
   show (SpellType t)        = t
   show (ArtifactType t)     = t
   show (EnchantmentType t)  = t
   show (PlaneswalkerType t) = t

data Rarity = Common
            | Uncommon
            | Rare
            | MythicRare
            deriving (Eq, Show, Read)

data Power = Power Int
           | UndecidablePower
           deriving (Eq)

instance Show Power where
   show (Power x)        = show x
   show UndecidablePower = "*"

data Toughness = Toughness Int
               | UndecidableToughness
               deriving (Eq)

instance Show Toughness where
   show (Toughness x)        = show x
   show UndecidableToughness = "*"

data Card = Card
   { cardName       :: String
   , cardManaCost   :: ManaCost
   , cardTypes      :: [CardType]
   , cardSupertypes :: [Supertype]
   , cardSubtypes   :: [Subtype]
   , cardText       :: String
   , cardFlavorText :: String
   , cardPT         :: Maybe (Power, Toughness)
   , cardLoyalty    :: Maybe Int
   , cardRarity     :: Rarity
   , cardID         :: Integer
   , cardSetID      :: (CardSet, Integer)
   }
   deriving (Eq, Show)

instance Ord Card where
   compare x y = compare (cardName x) (cardName y)

data RealCard = RealCard
   { cardInfo :: Card
   , isFoil   :: Bool
   } deriving (Eq, Ord, Show)

foil :: Card -> RealCard
foil = flip RealCard True

nonFoil :: Card -> RealCard
nonFoil = flip RealCard False

withRarity :: Rarity -> Card -> Bool
withRarity x = (==x) . cardRarity

withCardType :: CardType -> Card -> Bool
withCardType x = elem x . cardTypes

withSupertype :: Supertype -> Card -> Bool
withSupertype x = elem x . cardSupertypes

withSubtype :: Subtype -> Card -> Bool
withSubtype x = elem x . cardSubtypes

toSubtype :: String -> Subtype
toSubtype t = CreatureType t

cardColors :: Card -> CombColor
cardColors = manaCostCombColor . cardManaCost

cardSet :: Card -> CardSet
cardSet = fst . cardSetID

showTypes :: Card -> String
showTypes card
   | null st   = ct
   | otherwise = ct ++ " -- " ++ st
   where
      ct = intercalate " " $ map show (cardSupertypes card) ++ map show (cardTypes card)
      st = intercalate " " $ map show (cardSubtypes card)

showPT :: Card -> String
showPT card = case cardPT card of
   Nothing -> ""
   Just pt -> show (fst pt) ++ "/" ++ show (snd pt)

-- vim: set expandtab:
