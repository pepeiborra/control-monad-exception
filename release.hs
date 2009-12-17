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

main = do
  args <- getArgs
  real <- case args of
              [] -> return False
              ["really", "release"] -> return True
              _ -> do
                putStrLn "USAGE: release (really release)"
                exitWith ExitSuccess

-- Auxiliary functions
  let cmd = if real
              then \cmd -> do {putStrLn cmd; system cmd}
              else \cmd -> do {putStrLn cmd; return ExitSuccess}
      rename f f' = do
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

      saveCurrentCabalFiles = do
         currentDescriptors <- filter (\x -> takeExtension x ==".cabal")
                           <$> (getDirectoryContents =<< getCurrentDirectory)
         currentDescriptorsContents <- mapM BS.readFile currentDescriptors
         mapM (\x -> unless (x `elem` descriptors) $ rm x) currentDescriptors
         return (mapM (uncurry BS.writeFile) (zip currentDescriptors currentDescriptorsContents))

-- | Build, test, package and store for release.
--   Returns an action which does the actual uploading to Hackage.
      cabal_upload d = do
        version <- showVersion . pkgVersion . package . packageDescription
               <$> readPackageDescription Verbosity.normal d
        _ <- cmd "cabal clean"
        guardOk (d ++ " failed to build correctly")   =<< cmd "cabal install"
        guardOk (d ++ " failed to test correctly")    =<< cmd "cabal test"
        guardOk (d ++ " failed to package correctly") =<< cmd "cabal sdist"
        let packagedFilePath = dropExtension d ++ "-" ++ version <.> "tar.gz"
        copy ("dist" </> packagedFilePath) (releaseDir </> packagedFilePath)
        return $ do
          guardOk (d ++ " failed to upload correctly to Hackage") =<<
            cmd ("cabal upload " ++ releaseDir </> packagedFilePath)
          rm (releaseDir </> packagedFilePath)

-- THE ACTUAL SCRIPT

  restore <- saveCurrentCabalFiles
  createDir releaseDir
  (`finally` restore) . (`CE.catch` \e@SomeException{} -> print e >> putStrLn "aborting") $ do
    uploadActions <- forM descriptors $ \d -> do
      exists <- doesFileExist d
      let ext = takeExtension d
      if not exists
        then return(return ())
        else case ext of
        ".cabal" -> cabal_upload d <* rm d
        ".pp" -> do
           let d' = dropExtension d
           copy d d'
           action <- cabal_upload d'
           rm d'
           return action

    released <- length `liftM` sequence uploadActions
    putStrLn ("Release of " ++ show released ++ " packages completed.")
    rmDir releaseDir

ignore _ = return ()

guardOk msg (ExitFailure _) = do
  putStrLn msg
  exitWith (ExitFailure 1)

guardOk _ ExitSuccess = return ()
