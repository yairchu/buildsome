module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , startAlloc
  , withReleased
  , kill
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (ThreadId, myThreadId, killThread)
import Control.Monad (join)
import Data.IORef
import Data.Map (Map)
import Lib.IORef (atomicModifyIORef'_)
import Lib.PoolAlloc (PoolAlloc)
import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int
type Cell = IORef ParId

data ParallelismState = Live (Map ParId ThreadId) | Killed

data Parallelism = Parallelism
  { parPool :: PoolAlloc ParId
  , _parState :: IORef ParallelismState
  }

new :: ParId -> IO Parallelism
new n = Parallelism <$> PoolAlloc.new [1..n] <*> newIORef (Live M.empty)

finalizeAlloc :: Parallelism -> ParId -> IO ()
finalizeAlloc (Parallelism pool parState) parId = do
  threadId <- myThreadId
  join $ atomicModifyIORef' parState $ \old -> case old of
    Live m -> (Live (M.insert parId threadId m), return ())
    Killed -> (Killed, PoolAlloc.release pool parId >> E.throwIO E.ThreadKilled)

startAlloc :: Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc parallelism = do
  alloc <- PoolAlloc.startAlloc $ parPool parallelism
  return $ E.bracket (newCell =<< alloc) (release parallelism)
  where
    newCell parId = do
      finalizeAlloc parallelism parId
      newIORef parId

release :: Parallelism -> Cell -> IO ()
release (Parallelism pool parState) cell = do
  parId <- readIORef cell
  E.mask_ $ do
    PoolAlloc.release pool parId
    atomicModifyIORef'_ parState $ \old -> case old of
      Live m -> Live (M.delete parId m)
      Killed -> Killed
    writeIORef cell $ error "Attempt to read released resource"

-- | Release the currently held item, run given action, then regain
-- new item instead
withReleased :: Cell -> Parallelism -> IO b -> IO b
withReleased cell parallelism =
  E.bracket_ (release parallelism cell) alloc
  where
    alloc = E.mask_ $ do
      parId <- PoolAlloc.alloc $ parPool parallelism
      finalizeAlloc parallelism parId
      writeIORef cell parId

kill :: Parallelism -> IO ()
kill (Parallelism _ parState) =
  join $ atomicModifyIORef' parState $ \old -> case old of
  Live m -> (Killed, mapM_ killThread $ M.elems m)
  Killed -> (Killed, return ())
