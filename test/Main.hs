{-# LANGUAGE TupleSections #-}
import Magic.Card
import Magic.Reader
import Magic.BoosterPack
import Magic.MWDeck
import System.Directory
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as Map

main = do
   files <- map (prefix++) . filter isCardFile <$> getDirectoryContents prefix
   db  <- liftM (Map.fromList . catMaybes) $ forM files $ \file -> do
      mcard <- readCardFile file
      return $ liftM (\c -> (cardID c, c)) mcard
   let cards = Map.elems $ Map.filter ((=="Return to Ravnica") . cardSetName . cardSet) db
   putStrLn $ show (length cards) ++ " cards parsed."
   pack <- genPack cards
   mapM_ p pack
   where
      prefix = "data/card/"
      p (x,f) = do
         putStr (cardName x)
         when f $ putStr "(Foil)"
         putStrLn ""

isCardFile []      = False
isCardFile ('.':_) = False
isCardFile _       = True

-- vim: set expandtab:
