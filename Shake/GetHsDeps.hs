{-# LANGUAGE CPP #-}
module Shake.GetHsDeps (getHsDeps)
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO.Extra
import System.FilePath
import System.Process (callProcess)
import Text.ParserCombinators.ReadP

-- | Given a Haskell source file name, return a list of all local
-- Haskell source files it depends on, as given by 'ghc -M'.
getHsDeps :: FilePath -> IO [FilePath]
getHsDeps mainIs = do
  withTempDir $ \dir -> do
    let tmpFile = dir </> ".depend"
    callProcess "ghc" ["-dep-suffix", "", "-M", mainIs, "-dep-makefile", tmpFile]
    deps <- nub . catMaybes . map (mfilter (".hs" `isSuffixOf`))
            . map (fmap snd . parseLine) . trimLines . lines <$> readFile' tmpFile
    return deps

-- | Filter out comments.
trimLines :: [String] -> [String]
trimLines = filter isValidLine
  where
    isValidLine ('#':_) = False
    isValidLine _       = True

-- | Parse a line in 'ModuleName : ModuleName' format.
parseLine :: String -> Maybe (String, String)
parseLine l = case [ r | (r, rest) <- readP_to_S parser l, all isSpace rest] of
  []  -> Nothing
  [r] -> Just r
  _   -> Nothing
  where
    parser = do { skipSpaces; tgt <- parseModuleName
                ; skipSpaces; void $ char ':'
                ; skipSpaces; dep <- parseModuleName
                ; skipSpaces; return (tgt,dep) }

parseModuleName :: ReadP String
parseModuleName = munch1 (\c -> isAlphaNum c || c == '.'
                                || c == '-'  || c == '/' || c == '_')
