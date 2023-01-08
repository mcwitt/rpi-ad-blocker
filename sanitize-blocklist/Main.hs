{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, optional, (<|>))
import Data.Attoparsec.Text.Lazy qualified as P
import Data.Char (isAscii, isLetter)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO

newtype Host = Host Text

parseBlockedHosts :: Text -> Either String [Host]
parseBlockedHosts = P.parseOnly (blockedHosts <* optional "\n" <* P.endOfInput)
  where
    blockedHosts = fmap catMaybes maybeHosts
    maybeHosts = line `P.sepBy` "\n"
    line = comment <|> blockedHost
    comment = "#" *> P.skipWhile (not . P.isEndOfLine) $> Nothing
    blockedHost = "address=/" *> host <* "/#"
    host = Just . Host . T.intercalate "." <$> label `P.sepBy` "."
    label =
      let headChar = asciiLetter <|> P.digit <|> P.char '_'
          tailChar = headChar <|> P.char '-'
          p = (:) <$> headChar <*> many tailChar
       in fmap T.pack p
    asciiLetter = P.satisfy (\c -> isAscii c && isLetter c)

mkConfig :: [Host] -> Text
mkConfig = T.unlines . fmap mkBlockHost
  where
    mkBlockHost (Host hostname) = "address=/" <> hostname <> "/#"

sanitize :: Text -> Text
sanitize =
  mkConfig
    . fromRight (error "failed to parse blocked hosts")
    . parseBlockedHosts

main :: IO ()
main = TIO.interact sanitize
