module Magic.MWDeck
   ( writeAsMWDeck
   ) where

import Magic.Card
import System.IO
import Control.Monad
import Data.List

writeAsMWDeck :: FilePath -> [Card] -> IO ()
writeAsMWDeck path cards = do
   h <- openFile path WriteMode
   forM_ card' $ \c -> do
      putStrLn $ "    " ++ show (length c) ++ " [RTR] " ++ cardName (head c)
      hPutStrLn h $ "    " ++ show (length c) ++ " [RTR] " ++ cardName (head c)
   hClose h
   where
      card' = group $ sort cards

-- vim: set expandtab:

