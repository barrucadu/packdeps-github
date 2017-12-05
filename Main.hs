{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Either           (lefts, rights)
import qualified Distribution.PackDeps as P
import           System.Environment    (getArgs)

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Version  (showVersion)
#else
import           Data.Version          (showVersion)
#endif

main :: IO ()
main = getArgs >>= \case
  [fname] -> do
    cfgstr   <- readFileOrStdin fname
    snapshot <- P.loadNewest
    let cfg = toConfig snapshot cfgstr
    either (mapM_ putStrLn) (mapM_ (checkPackage snapshot)) cfg
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
  , info :: P.DescInfo
  -- ^ Information from the Hackage snapshot.
  } deriving Show

-- | Turn a configuration string into a list of packages to check.
toConfig :: P.Newest -> String -> Either [String] [Package]
toConfig snapshot = validate . map go . filter (not . isComment) . lines where
  go line = case words line of
    [name, userrepo] -> case P.getPackage name snapshot of
      Just di -> Right Package { hackage = name, github = userrepo, info = di }
      Nothing -> Left ("Unknown Hackage package: '" ++ name ++ "'")
    _ -> Left ("Confusing configuration entry: '" ++ line ++ "'")

  -- strip empty lines or lines starting with a #.
  isComment ('#':_) = True
  isComment "" = True
  isComment _  = False

-- | Check a package's dependencies are up-to-date.
checkPackage :: P.Newest -> Package -> IO ()
checkPackage snapshot pkg = putStrLn $ case P.checkDeps snapshot (info pkg) of
    (_, ver, P.AllNewest) -> pkgname ver ++ " is up-to-date"
    (_, ver, P.WontAccept ds _) -> pkgname ver ++ " is behind on " ++ unwords (map pkgname' ds)
  where
    pkgname ver = hackage pkg ++ "-" ++ showVersion ver
    pkgname' (name, ver) = name ++ "-" ++ ver

-- | Accumulate a list of possible errors.
validate :: [Either a b] -> Either [a] [b]
validate es = case (lefts es, rights es) of
  ([], bs) -> Right bs
  (as, _)  -> Left  as
