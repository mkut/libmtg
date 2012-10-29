import Magic.Reader
import Magic.BoosterPack
import Magic.MWDeck

main = do
   cs <- readCardsFile "data/rtr.txt"
   putStrLn $ show (length cs) ++ " cards parsed."
   cards <- genPack cs
   writeAsMWDeck "hoge.mwdeck" cards

-- vim: set expandtab:
