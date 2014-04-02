module Lib.Chart
  ( make
  ) where

import Control.Monad (void)
import Data.Default.Class (def)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as ChartCairo
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.Slave as Slave

buildTimes :: Slave.Stats -> Chart.PieChart
buildTimes (Slave.Stats stats) =
 def { Chart._pie_data = dataPoints }
 where
  dataPoints =
    let f (targetRep, (_when, count)) =
          def { Chart._pitem_label = BS8.unpack $ BuildMaps.targetRepPath targetRep
              , Chart._pitem_value = realToFrac count }
    in map f $ M.toList stats

make :: Slave.Stats -> FilePath -> IO ()
make stats filePath =
  void $ ChartCairo.renderableToFile fileOptions (Chart.toRenderable plot) $ BS8.unpack filePath
  where
    fileOptions = def
      { ChartCairo._fo_format = ChartCairo.SVG
      , ChartCairo._fo_size = (2048, 2048)
      }
    plot = def { Chart._pie_plot = buildTimes stats }
