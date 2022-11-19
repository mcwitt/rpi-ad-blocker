{ ghcWithPackages, runCommand, writeText }:

let
  ghc = ghcWithPackages (ps: with ps; [
    attoparsec
    text
  ]);

  code = writeText "Main.hs" ''
    {-# LANGUAGE ImportQualifiedPost #-}
    {-# LANGUAGE LambdaCase #-}
    {-# LANGUAGE OverloadedStrings #-}

    module Main where

    import Control.Applicative (many, optional, (<|>))
    import Data.Attoparsec.Text.Lazy
      ( char,
        digit,
        endOfInput,
        endOfLine,
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
    parseBlockedHosts = parseOnly (blockedHosts <* optional endOfLine <* endOfInput)
      where
        blockedHosts = fmap catMaybes maybeHosts
        maybeHosts = (comment <|> blockedHost) `sepBy` endOfLine
        comment = "#" *> skipWhile (not . isEndOfLine) $> Nothing
        blockedHost = "address=/" *> host <* "/#"
        host = Just . Host . T.intercalate "." <$> label `sepBy` "."
        label = do
          let headChar = asciiLetter <|> digit <|> char '_'
          h <- headChar
          t <- many (headChar <|> char '-')
          pure $ T.pack (h : t)
        asciiLetter = satisfy (\c -> isAscii c && isLetter c)

    mkConfig :: [Host] -> Text
    mkConfig = T.intercalate "\n" . fmap mkBlockHost
      where
        mkBlockHost (Host hostname) = "address=/" <> hostname <> "/#"

    liftEitherIO :: Either String a -> IO a
    liftEitherIO = \case
      Right a -> pure a
      Left e -> fail e

    main :: IO ()
    main = do
      cfg <- TIO.getContents
      blockedHosts <- liftEitherIO (parseBlockedHosts cfg)
      TIO.putStrLn (mkConfig blockedHosts)
  '';

in
runCommand "sanitize-blocklist" { } ''
  ${ghc}/bin/ghc -O -Wall -Werror ${code} -o $out
''
