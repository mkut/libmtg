import Magic.Reader
import Magic.BoosterPack

main = do
   cs <- readCardsFile "data/rtr.txt"
   putStrLn $ show (length cs) ++ " cards parsed."
   mapM_ print =<< genPack cs

-- vim: set expandtab:
