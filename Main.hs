{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Either        (lefts, rights)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  [fname] -> toConfig <$> readFileOrStdin fname >>= \case
    Right packages -> print packages
    Left errs -> mapM_ putStrLn errs
  _ -> putStrLn "USAGE: packdeps-github (- | /path/to/file)"

-- | Read stdin if the argument is \"-\", otherwise read the given
-- file.
readFileOrStdin :: String -> IO String
readFileOrStdin "-" = getContents
readFileOrStdin fname = readFile fname

-- | A package.
data Package = Package
  { hackage :: String
  -- ^ The name on hackage.
  , github :: String
  -- ^ The GitHub user and repository name.
  } deriving Show

-- | Turn a configuration string into a list of packages to check.
toConfig :: String -> Either [String] [Package]
toConfig = validate . map go . filter (not . isComment) . lines where
  go line = case words line of
    [name, userrepo] -> Right Package { hackage = name, github = userrepo }
    _ -> Left ("Confusing configuration entry: '" ++ line ++ "'")

  -- strip empty lines or lines starting with a #.
  isComment ('#':_) = True
  isComment "" = True
  isComment _  = False

-- | Accumulate a list of possible errors.
validate :: [Either a b] -> Either [a] [b]
validate es = case (lefts es, rights es) of
  ([], bs) -> Right bs
  (as, _)  -> Left  as
