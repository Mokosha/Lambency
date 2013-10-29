module Graphics.Rendering.Lambency.Utils (
  getLambencyDir
) where
--------------------------------------------------------------------------------
import qualified Config
import qualified System.Info
import Data.List
import Distribution.InstalledPackageInfo
import GHC.Paths
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error
--------------------------------------------------------------------------------
fixDescription :: String -> String
fixDescription s = let
  desc = idxOf "description:"
  cgry = idxOf "category:"
  in
   unlines $ take (cgry - desc) . drop desc $ lines s
  where
    idxOf s = case (findIndex (\x -> s `isPrefixOf` x) (lines s)) of
      Nothing -> 0
      Just sth -> sth

getPkgInfo :: FilePath -> IO InstalledPackageInfo
getPkgInfo fp = do
  putStrLn $ "Reading package info: " ++ fp
  fileContents <- readFile fp
  putStrLn $ fixDescription fileContents
  putStrLn $ show $ sourcePackageId ((read fileContents) :: InstalledPackageInfo)
  return . read =<< readFile fp

getPkgInfos :: IO [InstalledPackageInfo]
getPkgInfos = do

  global_conf_dir <- do let dir = takeDirectory $ takeDirectory ghc_pkg
                            path = dir </> "package.conf.d"
                        exists <- doesDirectoryExist path
                        if exists then return path
                          else ioError $ userError "Can't find package.conf.d"

  global_confs <- do files <- getDirectoryContents global_conf_dir
                     return $ getConfFiles global_conf_dir files

  user_confs <- try (getAppUserDataDirectory "ghc") >>= either
                (\_ -> return [])
                (\appdir -> do
                    let subdir = currentArch ++ '-' :currentOS ++ '-' :ghcVersion
                        user_conf = appdir </> subdir </> "package.conf.d"
                    user_exists <- doesDirectoryExist user_conf
                    files <- if user_exists then getDirectoryContents user_conf else return []
                    return $ getConfFiles user_conf files)
  let pkg_dbs = user_confs ++ global_confs
  mapM getPkgInfo pkg_dbs
  where
    getConfFiles dir fs = [ dir ++ '/' : f | f <- fs, isSuffixOf ".conf" f]

currentArch = System.Info.arch
currentOS = System.Info.os
ghcVersion = Config.cProjectVersion

getLambencyDir :: IO FilePath
getLambencyDir = do
  pkgDBs <- getPkgInfos
  return $ head $ libraryDirs $ head $ filter ((== "lambency") . show . installedPackageId) pkgDBs