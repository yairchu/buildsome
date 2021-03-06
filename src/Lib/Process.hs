module Lib.Process (getOutputs, Env, CmdSpec(..)) where

import Control.Concurrent.Async
import Control.Monad
import Data.Foldable (traverse_)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

type Env = [(String, String)]

-- | Create a process, and kill it when leaving the given code section
withProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withProcess params =
  E.bracket (createProcess params) terminate
  where
    terminate (mStdin, mStdout, mStderr, processHandle) = do
      (traverse_ . traverse_) hClose [mStdin, mStdout, mStderr]
      terminateProcess processHandle

-- | Get the outputs of a process with a given environment spec
getOutputs :: CmdSpec -> [String] -> Env -> IO (ExitCode, BS.ByteString, BS.ByteString)
getOutputs cmd inheritedEnvs envs = do
  oldEnvs <- forM inheritedEnvs $ \name -> do
    val <- getEnv name
    return (name, val)
  withProcess
    -- A hacky way to get a default CreateProcess record for "process" package version compatability:
    (shell "")
    { cwd = Nothing
    , cmdspec = cmd
    , env = Just (oldEnvs ++ envs)
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = False
    , create_group = True
--    , delegate_ctlc = True
    } $ \(Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) -> do
    hClose stdinHandle
    withAsync (BS.hGetContents stdoutHandle) $ \stdoutReader ->
      withAsync (BS.hGetContents stderrHandle) $ \stderrReader -> do
        exitCode <- waitForProcess processHandle
        stdout <- wait stdoutReader
        stderr <- wait stderrReader
        return (exitCode, stdout, stderr)
