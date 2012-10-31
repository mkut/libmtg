{-# LANGUAGE FlexibleContexts #-}
module Magic.Reader
   ( readCardsFile
   , readCardFile

   , parseTypes
   , parseCardType
   , parseSupertype
   , parseSubtype
   , parseRarity
   , parsePT
   , parseLoyalty
   ) where

import Magic.Card
import Magic.Color
import Magic.Mana
import Milib.IO (number')
import Prelude
import System.IO
import Control.Monad
import Text.Parsec

readCardsFile :: FilePath -> IO [Card]
readCardsFile fname = do
   toCards =<< hGetContents =<< openFile fname ReadMode

toCards :: String -> IO [Card]
toCards s = case parse (many parseCard) "" s of
   Left err -> do { print err; return [] }
   Right x  -> return x

readCardFile :: FilePath -> IO (Maybe Card)
readCardFile fname = do
   s <- hGetContents =<< openFile fname ReadMode
   case parse parseCard fname s of
      Left err -> do { print err; return Nothing }
      Right x  -> return (Just x)

-- Card
parseCard :: Stream s m Char => ParsecT s u m Card
parseCard = do
   name          <- manyTill anyChar newline
   cost          <- liftM ManaCost $ manyTill parseManaSymbol newline
   (ct, spt, st) <- withNewline parseTypes
   rarity        <- withNewline parseRarity
   text          <- manyTill anyChar separator
   ftext         <- manyTill anyChar separator
   pt            <- if elem Creature ct
                       then liftM Just $ withNewline parsePT
                       else return Nothing
   loyalty       <- if elem Planeswalker ct
                       then liftM Just $ withNewline parseLoyalty
                       else return Nothing
   gid           <- withNewline parseGID
   sid           <- withNewline parseSID
   return $ Card
      { cardName       = name
      , cardManaCost   = cost
      , cardTypes      = ct
      , cardSupertypes = spt
      , cardSubtypes   = st
      , cardText       = text
      , cardFlavorText = ftext
      , cardPT         = pt
      , cardLoyalty    = loyalty
      , cardRarity     = rarity
      , cardID         = gid
      , cardSetID      = sid
      }
   where
      separator = try $ string "=====\n"

-- Types
parseTypes :: Stream s m Char => ParsecT s u m ([CardType], [Supertype], [Subtype])
parseTypes = do
   spt <- many (try parseSupertype)
   ct <- many1 (try parseCardType)
   st <- option [] $ do
      string " - "
      many1 parseSubtype
   return (ct, spt, st)

parseCardType :: Stream s m Char => ParsecT s u m CardType
parseCardType = do
   skipMany (char ' ')
   choice
      [ do { _ <- string "Land"; return Land }
      , do { _ <- string "Creature"; return Creature }
      , do { _ <- string "Enchantment"; return Enchantment }
      , do { _ <- string "Artifact"; return Artifact }
      , do { _ <- string "Instant"; return Instant }
      , do { _ <- string "Sorcery"; return Sorcery }
      , do { _ <- string "Tribal"; return Tribal }
      , do { _ <- string "Planeswalker"; return Planeswalker }
      ]

parseSupertype :: Stream s m Char => ParsecT s u m Supertype
parseSupertype = do
   skipMany (char ' ')
   choice
      [ do { _ <- string "Basic"; return Basic }
      , do { _ <- string "Legendary"; return Legendary }
      , do { _ <- string "World"; return World }
      , do { _ <- string "Snow"; return Snow }
      , do { _ <- string "Ongoing"; return Ongoing }
      ]

parseSubtype :: Stream s m Char => ParsecT s u m Subtype
parseSubtype = do
   skipMany (char ' ')
   x <- many1 letter
   return $ toSubtype x

-- Rarity
parseRarity :: Stream s m Char => ParsecT s u m Rarity
parseRarity =   do { _ <- try (string "Common"); return Common }
            <|> do { _ <- try (string "Uncommon"); return Uncommon }
            <|> do { _ <- try (string "Rare"); return Rare }
            <|> do { _ <- try (string "MythicRare"); return MythicRare }

-- Number
parsePT :: Stream s m Char => ParsecT s u m (Power, Toughness)
parsePT = do
   p <- parsePower
   _ <- char '/'
   t <- parseToughness
   return (p, t)

parsePower :: Stream s m Char => ParsecT s u m Power
parsePower =   liftM Power number'
           <|> do { _ <- char '*'; return UndecidablePower }

parseToughness :: Stream s m Char => ParsecT s u m Toughness
parseToughness =   liftM Toughness number'
               <|> do { _ <- char '*'; return UndecidableToughness }

parseLoyalty :: Stream s m Char => ParsecT s u m Int
parseLoyalty = number'

-- Mana
parseManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseManaSymbol =   try parseGenericManaSymbol
                <|> try parseGenericManaSymbolX
                <|> try parseColorManaSymbol
                <|> try parseHybridManaSymbol
                <|> try parseMonocoloredHybridManaSymbox
                <|> try parsePhyrexianManaSymbox
                <?> "parseManaSymbol"

parseGenericManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseGenericManaSymbol = liftM GenericManaSymbol number'

parseGenericManaSymbolX :: Stream s m Char => ParsecT s u m ManaSymbol
parseGenericManaSymbolX = do
   _ <- char 'X'
   return GenericManaSymbolX

parseColorManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseColorManaSymbol = do
   c <- parseColor
   return $ ColorManaSymbol c

parseHybridManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseHybridManaSymbol = do
   _ <- char '{'
   c1 <- parseColor
   _ <- char '/'
   c2 <- parseColor
   _ <- char '}'
   case color2 c1 c2 of
      Just c  -> return $ HybridManaSymbol c
      Nothing -> fail "Same color at hybrid mana symbol."

parseMonocoloredHybridManaSymbox :: Stream s m Char => ParsecT s u m ManaSymbol
parseMonocoloredHybridManaSymbox = do
   _ <- char '{'
   _ <- char '2'
   _ <- char '/'
   c <- parseColor
   _ <- char '}'
   return $ MonocoloredHybridManaSymbol c

parsePhyrexianManaSymbox :: Stream s m Char => ParsecT s u m ManaSymbol
parsePhyrexianManaSymbox = do
   _ <- char '{'
   c <- parseColor
   _ <- char '/'
   _ <- char 'P'
   _ <- char '}'
   return $ PhyrexianManaSymbol c

-- Color
parseColor :: Stream s m Char => ParsecT s u m Color1
parseColor =   do { _ <- char 'W'; return White }
           <|> do { _ <- char 'U'; return Blue  }
           <|> do { _ <- char 'B'; return Black }
           <|> do { _ <- char 'R'; return Red   }
           <|> do { _ <- char 'G'; return Green }

-- ID
parseGID :: Stream s m Char => ParsecT s u m Integer
parseGID = do
   _ <- char '#'
   number'

parseSID :: Stream s m Char => ParsecT s u m (CardSet, Integer)
parseSID = do
   set <- liftM CardSet $ manyTill anyChar (char '#')
   sid <- number'
   return (set, sid)

-- Util
withNewline :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withNewline p = do
   r <- p
   _ <- newline
   return r

-- vim: set expandtab:
