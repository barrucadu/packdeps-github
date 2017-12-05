{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either             (lefts, rights)
import qualified Data.Foldable           as F
import           Data.Proxy              (Proxy(..))
import qualified Data.Text               as T
import qualified Distribution.PackDeps   as P
import qualified GitHub.Auth             as G
import qualified GitHub.Data.Definitions as G
import qualified GitHub.Data.Issues      as G
import qualified GitHub.Data.Name        as G
import qualified GitHub.Endpoints.Issues as G
import           System.Environment      (getArgs)

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Version    (showVersion)
#else
import           Data.Version            (showVersion)
#endif

-- | A package.
data Package = Package
  { hackage :: String
  -- ^ The name on hackage.
  , owner :: G.Name G.Owner
  -- ^ The GitHub user.
  , repo :: G.Name G.Repo
  -- ^ The GitHub repository.
  , info :: P.DescInfo
  -- ^ Information from the Hackage snapshot.
  } deriving Show

-- | GitHub credentials.  TODO: put this in the config file.
credentials :: G.Auth
credentials = G.BasicAuth "username" "password"

main :: IO ()
main = getArgs >>= \case
  [fname] -> do
    cfgstr   <- readFileOrStdin fname
    snapshot <- P.loadNewest
    let cfg = toConfig snapshot cfgstr
    either (mapM_ putStrLn) (mapM_ (checkPackage credentials snapshot)) cfg
  _ -> putStrLn "USAGE: packdeps-github (- | /path/to/file)"

-- | Read stdin if the argument is \"-\", otherwise read the given
-- file.
readFileOrStdin :: String -> IO String
readFileOrStdin "-" = getContents
readFileOrStdin fname = readFile fname

-------------------------------------------------------------------------------

-- | Turn a configuration string into a list of packages to check.
toConfig :: P.Newest -> String -> Either [String] [Package]
toConfig snapshot = validate . map go . filter (not . isComment) . lines where
  go line = case words line of
    [name, userrepo] -> case P.getPackage name snapshot of
      Just di ->
        let ostr = takeWhile (/='/') userrepo
            rstr = drop 1 (dropWhile (/='/') userrepo)
        in Right Package
           { hackage = name
           , owner   = G.mkName Proxy (T.pack ostr)
           , repo    = G.mkName Proxy (T.pack rstr)
           , info    = di
           }
      Nothing -> Left ("Unknown Hackage package: '" ++ name ++ "'")
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

-------------------------------------------------------------------------------

-- | Check a package's dependencies are up-to-date.
checkPackage :: G.Auth -> P.Newest -> Package -> IO ()
checkPackage auth snapshot pkg = case P.checkDeps snapshot (info pkg) of
    (_, ver, P.AllNewest) -> putStrLn $ pkgname ver ++ " is up-to-date"
    (_, ver, P.WontAccept ds _) -> do
      putStrLn $ pkgname ver ++ " is behind on " ++ unwords (map pkgname' ds)
      openIssue auth pkg (pkgname ver) (map pkgname' ds)
  where
    pkgname ver = hackage pkg ++ "-" ++ showVersion ver
    pkgname' (name, ver) = name ++ "-" ++ ver

-- | Open an issue on a repository.  Doesn't open an issue if another
-- with the same title exists.
openIssue :: G.Auth -> Package -> String -> [String] -> IO ()
openIssue auth pkg pkgname deps = G.issuesForRepo (owner pkg) (repo pkg) mempty >>= \case
    Right issues -> case filter (\i -> G.issueTitle i == title) (F.toList issues) of
      (issue:_) -> putStrLn $ "    pre-existing issue found: #" ++ show (G.issueNumber issue)
      [] -> G.createIssue auth (owner pkg) (repo pkg) issue >>= \case
        Right issue -> putStrLn $ "    opened issue #" ++ show (G.issueNumber issue)
        Left  err   -> putStrLn $ "    failed to open issue: " ++ show err
    Left err -> putStrLn $ "    failed to get list of issues: " ++ show err
  where
    title = issueTitle pkgname
    issue = G.NewIssue
      { G.newIssueTitle = title
      , G.newIssueBody  = Just (issueBody pkgname deps)
      , G.newIssueAssignee  = Nothing
      , G.newIssueMilestone = Nothing
      , G.newIssueLabels    = Just []
      }

-- | Title to use for issues.
issueTitle :: String -> T.Text
issueTitle pkgname = T.pack $
  "Dependencies for " ++ pkgname ++ " out of date"

-- | Template for an issue body.
issueBody :: String -> [String] -> T.Text
issueBody pkgname deps = T.pack . unlines $
  [ "Hi!"
  , ""
  , "Some of the dependencies of " ++ pkgname ++ " (the latest release on Hackage) are outdated:"
  , ""
  ] ++
  [" - [ ] " ++ dep | dep <- deps] ++
  [ ""
  , "This is an automatically-opened issue."
  ]
