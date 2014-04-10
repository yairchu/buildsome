module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , startAlloc
  , withReleased
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (ThreadId, myThreadId)
import Data.IORef
import Data.Map (Map)
import Lib.IORef (atomicModifyIORef'_)
import Lib.PoolAlloc (PoolAlloc)
import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int
type Cell = IORef ParId

data Parallelism = Parallelism
  { _parPool :: PoolAlloc ParId
  , _parThreads :: IORef (Map ParId ThreadId)
  }

new :: ParId -> IO Parallelism
new n = Parallelism <$> PoolAlloc.new [1..n] <*> newIORef M.empty

addMyThreadId :: IORef (Map ParId ThreadId) -> ParId -> IO ()
addMyThreadId threads parId = do
  threadId <- myThreadId
  atomicModifyIORef'_ threads $ M.insert parId threadId

startAlloc :: Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc parallelism@(Parallelism pool threads) = do
  alloc <- PoolAlloc.startAlloc pool
  return $ E.bracket (newCell =<< alloc) (release parallelism)
  where
    newCell parId = do
      addMyThreadId threads parId
      newIORef parId

release :: Parallelism -> Cell -> IO ()
release (Parallelism pool threads) cell = do
  parId <- readIORef cell
  E.mask_ $ do
    PoolAlloc.release pool parId
    atomicModifyIORef'_ threads $ M.delete parId
    writeIORef cell $ error "Attempt to read released resource"

-- | Release the currently held item, run given action, then regain
-- new item instead
withReleased :: Cell -> Parallelism -> IO b -> IO b
withReleased cell parallelism@(Parallelism pool threads) =
  E.bracket_ (release parallelism cell) alloc
  where
    alloc = E.mask_ $ do
      parId <- PoolAlloc.alloc pool
      addMyThreadId threads parId
      writeIORef cell parId
