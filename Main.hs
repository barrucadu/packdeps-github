{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson.Types        as J
import qualified Data.ByteString         as BS
import           Data.Either             (lefts, rights)
import qualified Data.Foldable           as F
import           Data.Proxy              (Proxy(..))
import           Data.String             (fromString)
import qualified Data.Text               as T
import qualified Data.Yaml               as Y
import qualified Distribution.PackDeps   as P
import           GHC.Generics            (Generic)
import qualified GitHub.Auth             as G
import qualified GitHub.Data.Definitions as G
import qualified GitHub.Data.Issues      as G
import qualified GitHub.Data.Name        as G
import qualified GitHub.Endpoints.Issues as G
import           System.Environment      (getArgs)

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Version    (Version, showVersion)
#else
import           Data.Version            (Version, showVersion)
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

main :: IO ()
main = getArgs >>= \case
  [fname] -> do
    cfgstr   <- readFileOrStdin fname
    snapshot <- P.loadNewest
    case toConfig snapshot <$> Y.decodeEither cfgstr of
      Right (auth, cfg, tpl) ->
        either (mapM_ putStrLn) (mapM_ (checkPackage auth snapshot tpl)) cfg
      Left err -> putStrLn ("Could not parse configuration file: " ++ err)
  _ -> putStrLn "USAGE: packdeps-github (- | /path/to/file)"

-- | Read stdin if the argument is \"-\", otherwise read the given
-- file.
readFileOrStdin :: String -> IO BS.ByteString
readFileOrStdin "-" = BS.getContents
readFileOrStdin fname = BS.readFile fname

-------------------------------------------------------------------------------

-- | Turn a configuration string into a list of packages to check.
toConfig :: P.Newest -> Config -> (G.Auth, Either [String] [Package], String -> Version -> [String] -> G.NewIssue)
toConfig snapshot cfg = (auth, packages, tpl) where
  auth =
    let user = cfgAusername (cfgCgithub cfg)
        pass = cfgApassword (cfgCgithub cfg)
    in G.BasicAuth (fromString user) (fromString pass)

  packages = validate (map go (cfgCpackages cfg))
  go pkg = case P.getPackage (cfgPpackage pkg) snapshot of
    Just di ->
      let ostr = takeWhile (/='/') (cfgPgithub pkg)
          rstr = drop 1 (dropWhile (/='/') (cfgPgithub pkg))
      in Right Package
           { hackage = cfgPpackage pkg
           , owner   = G.mkName Proxy (T.pack ostr)
           , repo    = G.mkName Proxy (T.pack rstr)
           , info    = di
           }
    Nothing -> Left ("Unknown Hackage package: '" ++ cfgPpackage pkg ++ "'")

  tpl pkgname version depslist =
    let titletpl = cfgTtitle (cfgCissueTemplate cfg)
        bodytpl  = cfgTbody  (cfgCissueTemplate cfg)
        fill = T.replace "{{package}}"      (T.pack pkgname)
             . T.replace "{{version}}"      (T.pack (showVersion version))
             . T.replace "{{dependencies}}" (T.pack (unlines ["- [ ] " ++ d | d <- depslist]))
    in G.NewIssue
       { G.newIssueTitle = fill titletpl
       , G.newIssueBody  = Just (fill bodytpl)
       , G.newIssueAssignee  = Nothing
       , G.newIssueMilestone = Nothing
       , G.newIssueLabels    = Just []
       }

-- | Accumulate a list of possible errors.
validate :: [Either a b] -> Either [a] [b]
validate es = case (lefts es, rights es) of
  ([], bs) -> Right bs
  (as, _)  -> Left  as

-------------------------------------------------------------------------------

-- | Check a package's dependencies are up-to-date.
checkPackage :: G.Auth -> P.Newest -> (String -> Version -> [String] -> G.NewIssue) -> Package -> IO ()
checkPackage auth snapshot tpl pkg = case P.checkDeps snapshot (info pkg) of
    (_, ver, P.AllNewest) -> putStrLn $ pkgname ver ++ " is up-to-date"
    (_, ver, P.WontAccept ds _) -> do
      putStrLn $ pkgname ver ++ " is behind on " ++ unwords (map pkgname' ds)
      openIssue auth pkg (tpl (hackage pkg) ver (map pkgname' ds))
  where
    pkgname ver = hackage pkg ++ "-" ++ showVersion ver
    pkgname' (name, ver) = name ++ "-" ++ ver

-- | Open an issue on a repository.  Doesn't open an issue if another
-- with the same title exists.
openIssue :: G.Auth -> Package -> G.NewIssue -> IO ()
openIssue auth pkg issue = G.issuesForRepo (owner pkg) (repo pkg) mempty >>= \case
  Right issues -> case filter (\i -> G.issueTitle i == G.newIssueTitle issue) (F.toList issues) of
    (issue:_) -> putStrLn $ "    pre-existing issue found: #" ++ show (G.issueNumber issue)
    [] -> G.createIssue auth (owner pkg) (repo pkg) issue >>= \case
      Right issue -> putStrLn $ "    opened issue #" ++ show (G.issueNumber issue)
      Left  err   -> putStrLn $ "    failed to open issue: " ++ show err
  Left err -> putStrLn $ "    failed to get list of issues: " ++ show err

-------------------------------------------------------------------------------

-- | A parsed configuration file.
data Config = Config
  { cfgCgithub :: CfgAuth
  -- ^ GitHub username and password.
  , cfgCpackages :: [CfgPackage]
  -- ^ Packages to check.
  , cfgCissueTemplate :: CfgTemplate
  -- ^ Issue template.
  } deriving (Show, Generic)

-- | GitHub credentials from the configuration file.
data CfgAuth = CfgAuth
  { cfgAusername :: String
  , cfgApassword :: String
  } deriving (Show, Generic)

-- | Package from the configuration file.
data CfgPackage = CfgPackage
  { cfgPpackage :: String
  , cfgPgithub :: String
  } deriving (Show, Generic)

-- | Issue template from the configuration file.
data CfgTemplate = CfgTemplate
  { cfgTtitle :: T.Text
  , cfgTbody  :: T.Text
  } deriving (Show, Generic)

instance Y.FromJSON Config where
  parseJSON = J.genericParseJSON yopts

instance Y.FromJSON CfgAuth where
  parseJSON = J.genericParseJSON yopts

instance Y.FromJSON CfgPackage where
  parseJSON = J.genericParseJSON yopts

instance Y.FromJSON CfgTemplate where
  parseJSON = J.genericParseJSON yopts

-- | Options for yaml decoding
yopts :: J.Options
#if MIN_VERSION_aeson(1,2,0)
yopts = J.fieldLabelModifier J.defaultOptions (J.camelTo2 '_' . drop 4)
#else
yopts = J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop 4 }
#endif
