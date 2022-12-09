{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, optional, (<|>))
import Data.Attoparsec.Text.Lazy
  ( char,
    digit,
    endOfInput,
    isEndOfLine,
    parseOnly,
    satisfy,
    sepBy,
    skipWhile,
  )
import Data.Char (isAscii, isLetter)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO

newtype Host = Host Text

parseBlockedHosts :: Text -> Either String [Host]
parseBlockedHosts = parseOnly (blockedHosts <* optional "\n" <* endOfInput)
  where
    blockedHosts = fmap catMaybes maybeHosts
    maybeHosts =  line `sepBy` "\n"
    line = comment <|> blockedHost
    comment = "#" *> skipWhile (not . isEndOfLine) $> Nothing
    blockedHost = "address=/" *> host <* "/#"
    host = Just . Host . T.intercalate "." <$> label `sepBy` "."
    label =
      let headChar = asciiLetter <|> digit <|> char '_'
          tailChar = headChar <|> char '-'
          p = (:) <$> headChar <*> many tailChar
       in fmap T.pack p
    asciiLetter = satisfy (\c -> isAscii c && isLetter c)

mkConfig :: [Host] -> Text
mkConfig = T.unlines . fmap mkBlockHost
  where
    mkBlockHost (Host hostname) = "address=/" <> hostname <> "/#"

main :: IO ()
main = do
  cfg <- TIO.getContents
  blockedHosts <- either fail pure (parseBlockedHosts cfg)
  TIO.putStrLn (mkConfig blockedHosts)
