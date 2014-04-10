module Lib.FormatTime
  ( FormatTime
  , time
  , full
  , short
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Time (FormatTime)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Time as Time
import qualified System.Locale as Locale

time :: FormatTime t => String -> t -> ByteString
time fmt = BS8.pack . Time.formatTime Locale.defaultTimeLocale fmt

full :: FormatTime t => t -> ByteString
full = time "%y%m%d-%H%M%S%Q"

short :: FormatTime t => t -> ByteString
short x = time "%M:%S" x <> BS8.take 4 (time "%Q" x)
