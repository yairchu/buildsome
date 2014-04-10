{-# LANGUAGE DeriveGeneric #-}
module Lib.BuildId
  ( BuildId, new
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import qualified Lib.FormatTime as FormatTime

newtype BuildId = BuildId ByteString
  deriving (Show, Generic)

instance Binary BuildId

new :: IO BuildId
new = BuildId . FormatTime.full <$> getCurrentTime
