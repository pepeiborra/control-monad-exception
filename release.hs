#!/usr/bin/env runHaskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Exception as CE
import Control.Monad
import qualified Data.ByteString as BS
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity as Verbosity
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

descriptors = ["control-monad-exception.cabal"
              ,"control-monad-exception-mtl.cabal.pp"
              ,"control-monad-exception-monadsfd.cabal.pp"
              ,"control-monad-exception-monadstf.cabal.pp"
              ]

releaseDir = "release"

data Action = Release | Test | Script deriving Eq

isRelease Release = True
isRelease Script  = True
isRelease Test    = False

isScript  Script  = True
isScript  _       = False

main = do
  args   <- getArgs
  action <- case args of
              [] -> return Script
              ["test"] -> return Test
              ["really", "release"] -> return Release
              _ -> do
                putStrLn "USAGE: release [test|really release]"
                exitWith ExitSuccess

  let real = not . isScript $ action
-- Auxiliary functions
  let cmd = case action of
            Release-> \cmd -> do {putStrLn cmd; system cmd}
            Test   -> \cmd -> do {putStrLn cmd; system cmd}
            Script -> \cmd -> do {putStrLn cmd; return ExitSuccess}
      mv f f' = do
         putStrLn ("mv " ++ f ++ " " ++ f')
         when real $ renameFile f f'

      rm f = do
         putStrLn ("rm " ++ f)
         when real $ removeFile f

      rmDir f = do
         putStrLn ("rmDir " ++ f)
         when real $ removeDirectory f

      copy f f' = do
         putStrLn ("cp " ++ f ++ " " ++ f')
         when real $ copyFile f f'

      createDir d = do
         putStrLn ("mkDir " ++ d)
         when real $ createDirectoryIfMissing True d

      stripExtension f = mv f (dropExtension f)
      cpTemp f = go [] where
         go tag = do
           let f' = f <.> "tmp" ++ if null tag then "" else show (head tag)
           exists <- doesFileExist f'
           if not exists then copy f f' *> pure f'
              else go (inc tag)
         inc [] = [1]
         inc [x] = [x+1]

      saveCurrentCabalFiles = do
         currentDescriptors <- filter (\x -> takeExtension x ==".cabal")
                           <$> (getDirectoryContents =<< getCurrentDirectory)
         currentDescriptorsTmp <- mapM cpTemp currentDescriptors
         return (mapM stripExtension currentDescriptorsTmp)

-- | build and test
      cabal_test d = do
        _ <- cmd "cabal clean -v0"
        guardOk (d ++ " failed to build correctly")   =<< cmd "cabal install -v0"
        guardOk (d ++ " failed to test correctly")    =<< cmd "cabal test"

-- | package and store for release.
      cabal_dist d = when (isRelease action) $ do
        guardOk (d ++ " failed to package correctly") =<< cmd ("cabal sdist --builddir=" ++ releaseDir)

-- | Returns an action to upload a package already stored in the release dir to Hackage
      cabal_upload d = do
        version <- showVersion . pkgVersion . package . packageDescription
                  <$> readPackageDescription Verbosity.normal d
        let packagedFilePath = (dropExtension.dropExtension) d ++ "-" ++ version <.> "tar.gz"
        return $
         if isRelease action
          then toOk (packagedFilePath ++ " failed to upload correctly to Hackage") <$>
                  cmd ("cabal upload " ++ releaseDir </> packagedFilePath)
               <* rm (releaseDir </> packagedFilePath)
          else return Ok

-- THE ACTUAL SCRIPT

  restore <- saveCurrentCabalFiles
  createDir releaseDir
  (`finally` restore) . (`CE.catch` \e@SomeException{} -> print e >> putStrLn "aborting") $ do
    uploadActions <- forM descriptors $ \d -> do
      exists <- doesFileExist d
      let ext = takeExtension d
      if exists || isScript action
        then cabal_upload d <*
             case ext of
               ".cabal" -> cabal_test d <* cabal_dist d <* rm d
               ".pp" -> let d' = dropExtension d in
                        copy d d' <* cabal_test d' <* cabal_dist d' <* rm d'
        else pure (pure (Fail d))


    unless (isScript action) $ putStrLn "Packages tested succesfully"
    when (isRelease action) $ do
      done <- sequence uploadActions
      unless (isScript action) $ do
          putStrLn ("Release of " ++ show (length $ filter isOk done) ++ " packages completed.")
          when (not $ all isOk done) $ do
               putStrLn $ "Warning: there were " ++ show(length$ filter (not.isOk) done)
                            ++ " packages which failed to release"
               forM_ (filter (not.isOk) done) $ \(Fail msg) -> putStrLn msg
      rmDir releaseDir

ignore _ = return ()

guardOk msg (ExitFailure _) = do
  putStrLn msg
  exitWith (ExitFailure 1)

guardOk _ ExitSuccess = return ()

data Ok = Ok | Fail {msg::String} deriving (Eq,Ord,Show)
toOk _   ExitSuccess = Ok
toOk msg _           = Fail msg

isOk Ok = True
isOk _  = False
