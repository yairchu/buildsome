{-# LANGUAGE OverloadedStrings #-}
module Lib.ScanFileUpwards (scanFileUpwards) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Lib.FilePath (FilePath, (</>))
import Prelude hiding (FilePath)
import qualified Lib.FilePath as FilePath
import qualified System.Posix.ByteString as Posix

scanFileUpwards :: FilePath -> IO (Maybe FilePath)
scanFileUpwards name = do
  cwd <- Posix.getWorkingDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate FilePath.takeDirectory cwd
    candidates = map (</> name) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runEitherT $ mapM_ check candidates
  case res of
    Left found -> Just <$> FilePath.makeRelativeToCurrentDirectory found
    Right () -> return $ Nothing
  where
    check path = do
      exists <- liftIO $ Posix.fileExist path
      when exists $ left path
