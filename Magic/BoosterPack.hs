module Magic.BoosterPack
   ( genPack
   ) where

import Magic.Card
import Prelude
import System.Random
import System.Random.Shuffle
import Control.Applicative ((<$>))
import Control.Monad

genPack :: [Card] -> IO [Card]
genPack cards = do
   isMythic <- (==1) <$> getStdRandom (randomR (1 :: Int, 8))
   isFoil <- (==1) <$> getStdRandom (randomR (1 :: Int, 4))
   f <- if isFoil then take 1 <$> shuffleM cards else return []
   r <- take 1 <$> shuffleM (if isMythic then mythic else rare)
   u <- take 3 <$> shuffleM uncommon
   c <- if isFoil then take 9 <$> shuffleM common else take 10 <$> shuffleM common
   l <- take 1 <$> shuffleM land
   return $ f ++ r ++ u ++ c ++ l
   where
      mythic   = filter (withRarity MythicRare) $ cards
      rare     = filter (withRarity Rare) $ cards
      uncommon = filter (withRarity Uncommon) $ cards
      common   = filter (not . fLand) . filter (withRarity Common) $ cards
      land     = filter fLand $ cards
      fLand x = withCardType Land x && withSupertype Basic x

-- vim: set expandtab:
