{-# LANGUAGE FlexibleContexts #-}
module Magic.Reader
   ( readCardsFile

   , parseName
   , parseManaCost
   , parseTypes
   , parseCardType
   , parseSupertype
   , parseSubtype
   , parseRarity
   , parseText
   , parseFlavorText
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

-- Card
parseCard :: Stream s m Char => ParsecT s u m Card
parseCard = do
   n <- parseName
   _ <- char '/'
   c <- parseManaCost
   _ <- newline
   (ct, spt, st) <- parseTypes
   _ <- char '/'
   r <- parseRarity
   _ <- newline
   _ <- string "=====\n"
   t <- parseText
   _ <- string "=====\n"
   ft <- parseFlavorText
   _ <- string "=====\n"
   mpt <- optionMaybe $ try $ withNewline parsePT
   ml <- optionMaybe $ try $ withNewline parseLoyalty
   _ <- newline
   return $ Card n c ct spt st t ft mpt ml r

-- Text
parseName :: Stream s m Char => ParsecT s u m String
parseName = many1 nameChar

nameChar :: Stream s m Char => ParsecT s u m Char
nameChar =   letter
         <|> digit
         <|> char ' '
         <|> char '\''
         <|> char ','
         <|> char '-'

parseText :: Stream s m Char => ParsecT s u m String
parseText = many textChar

parseFlavorText :: Stream s m Char => ParsecT s u m String
parseFlavorText = many textChar

textChar :: Stream s m Char => ParsecT s u m Char
textChar =   nameChar
         <|> char '\n'
         <|> char '/'
         <|> char '.'
         <|> char '?'
         <|> char '!'
         <|> char ';'
         <|> char ':'
         <|> char '"'
         <|> char '('
         <|> char ')'
         <|> char '{'
         <|> char '}'
         <|> char '+'

-- Types
parseTypes :: Stream s m Char => ParsecT s u m ([CardType], [Supertype], [Subtype])
parseTypes = do
   spt <- many (try parseSupertype)
   ct <- many1 (try parseCardType)
   st <- option [] $ do
      spaces
      _ <- char '-'
      many1 parseSubtype
   return (ct, spt, st)

parseCardType :: Stream s m Char => ParsecT s u m CardType
parseCardType = do
   spaces
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
   spaces
   choice
      [ do { _ <- string "Basic"; return Basic }
      , do { _ <- string "Legendary"; return Legendary }
      , do { _ <- string "World"; return World }
      , do { _ <- string "Snow"; return Snow }
      , do { _ <- string "Ongoing"; return Ongoing }
      ]

parseSubtype :: Stream s m Char => ParsecT s u m Subtype
parseSubtype = do
   spaces
   x <- many1 letter
   return $ toSubtype x

-- Rarity
parseRarity :: Stream s m Char => ParsecT s u m Rarity
parseRarity =   do { _ <- char 'C'; return Common }
            <|> do { _ <- char 'U'; return Uncommon }
            <|> do { _ <- char 'R'; return Rare }
            <|> do { _ <- char 'M'; return MythicRare }

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
parseManaCost :: Stream s m Char => ParsecT s u m ManaCost
parseManaCost = liftM ManaCost $ many parseManaSymbol

parseManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseManaSymbol =   try parseGenericManaSymbol
                <|> try parseGenericManaSymbolX
                <|> try parseColorManaSymbol
                <|> try parseHybridManaSymbol
                <|> try parseMonocoloredHybridManaSymbox
                <|> try parsePhyrexianManaSymbox
                <?> "parseManaSymbol"

parseGenericManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseGenericManaSymbol = do
   _ <- char '('
   n <- number'
   _ <- char ')'
   return $ GenericManaSymbol n

parseGenericManaSymbolX :: Stream s m Char => ParsecT s u m ManaSymbol
parseGenericManaSymbolX = do
   _ <- char '('
   _ <- char 'X'
   _ <- char ')'
   return GenericManaSymbolX

parseColorManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseColorManaSymbol = do
   _ <- char '('
   c <- parseColor
   _ <- char ')'
   return $ ColorManaSymbol c

parseHybridManaSymbol :: Stream s m Char => ParsecT s u m ManaSymbol
parseHybridManaSymbol = do
   _ <- char '('
   c1 <- parseColor
   _ <- char '/'
   c2 <- parseColor
   _ <- char ')'
   case color2 c1 c2 of
      Just c  -> return $ HybridManaSymbol c
      Nothing -> fail "Same color at hybrid mana symbol."

parseMonocoloredHybridManaSymbox :: Stream s m Char => ParsecT s u m ManaSymbol
parseMonocoloredHybridManaSymbox = do
   _ <- char '('
   _ <- char '2'
   _ <- char '/'
   c <- parseColor
   _ <- char ')'
   return $ MonocoloredHybridManaSymbol c

parsePhyrexianManaSymbox :: Stream s m Char => ParsecT s u m ManaSymbol
parsePhyrexianManaSymbox = do
   _ <- char '('
   c <- parseColor
   _ <- char '/'
   _ <- char 'P'
   _ <- char ')'
   return $ PhyrexianManaSymbol c

-- Color
parseColor :: Stream s m Char => ParsecT s u m Color1
parseColor =   do { _ <- char 'W'; return White }
           <|> do { _ <- char 'U'; return Blue  }
           <|> do { _ <- char 'B'; return Black }
           <|> do { _ <- char 'R'; return Red   }
           <|> do { _ <- char 'G'; return Green }

-- Util
withNewline :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withNewline p = do
   r <- p
   _ <- newline
   return r

-- vim: set expandtab:
