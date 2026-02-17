{-# LANGUAGE OverloadedStrings #-}

module Test.Stress.Driver.Summary
  ( createSummaryPlots
  , eventlogToSvg
  ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as BS.Lazy
import Data.List
import Data.Maybe
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy hiding ((<.>))
import System.Directory
import System.Exit
import System.FilePath

import Test.Stress.Common

createSummaryPlots :: Bool -> Maybe FilePath -> IO ()
createSummaryPlots v mwd = do
    cwd <- getCurrentDirectory
    let wd = fromMaybe cwd mwd
    putStrLn $ "Creating summary plots in " ++ wd ++ "..."
    wdFiles <- map (wd </>) <$> listDirectory wd
    let wdElFiles = filter (".eventlog" `isSuffixOf`) wdFiles
    say' v $ "found event logs:"
    mapM_ (say' v) $ map ("  " ++) wdElFiles
    mapM_ (\e -> handleFailure e $ eventlogToSvg v e) wdElFiles
  where
    handleFailure :: FilePath -> IO () -> IO ()
    handleFailure f =
        handle $ \case
          e | Just UserInterrupt <- fromException e ->
              exitFailure
            | otherwise -> do
              putStrLn $ "failed to generate summary plot for " ++ f
              print e

eventlogToSvg :: Bool -> FilePath -> IO ()
eventlogToSvg v elFile = do
    say' v $ "generating plot file for " ++ elFile
    elBytes <- BS.Lazy.readFile elFile
    case readEventLog elBytes of
      Right (EventLog _ (Data events), _merr) -> do
        samples <- goEvents emptySamples events
        toFile def (elFile <.> "svg") $ do
          layout_title .= elFile
          layout_x_axis . laxis_title .= "Time (seconds)"
          layout_y_axis . laxis_title .= "Size (megabytes)"
          plot (line "live bytes" [samplesLiveBytes samples])
          plot (line "blocks size" [samplesBlocksSize samples])
          plot (line "heap size" [samplesHeapSize samples])
        say' v $ "finished plot for " ++ elFile
      Left err ->
        putStrLn $
          "Failed to create summary plot from " ++ elFile ++ ": " ++ err
  where
    goEvents :: Samples -> [Event] -> IO Samples
    goEvents acc [] =
        return acc
    goEvents acc (Event t ei _:es) = do
        acc' <- addEvent acc (t,ei)
          `catch` (\(_e :: SomeException) -> return acc)
        goEvents acc' es

    addEvent :: Samples -> (Timestamp, EventInfo) -> IO Samples
    addEvent acc (t, ei) =
        case ei of
          HeapLive _ s -> return $ acc {
              samplesLiveBytes =
                insertBy byFirst
                  (timeConv t, sizeConv s) (samplesLiveBytes acc)
            }
          BlocksSize _ s -> return $ acc {
              samplesBlocksSize =
                insertBy byFirst
                  (timeConv t, sizeConv s) (samplesBlocksSize acc)
            }
          HeapSize _ s -> return $ acc {
              samplesHeapSize =
                insertBy byFirst
                  (timeConv t, sizeConv s) (samplesHeapSize acc)
            }
          _ -> return acc
      where
        timeConv :: Word64 -> Double
        sizeConv :: Word64 -> Double
        timeConv = (/ 1_000_000_000) . fromIntegral -- nanoseconds to seconds
        sizeConv = (/ 1_000_000)     . fromIntegral -- bytes to megabytes

say' :: Bool -> String -> IO ()
say' v = say v . ("(summary) " ++)

byFirst :: Ord a => (a, b) -> (a, b) -> Ordering
byFirst (x1, _) (x2, _) = compare x1 x2

-------------------------------------------------------------------------------
-- Internal auxiliary
-------------------------------------------------------------------------------

data Samples = Samples {
      samplesLiveBytes  :: [(Double, Double)]
    , samplesBlocksSize :: [(Double, Double)]
    , samplesHeapSize   :: [(Double, Double)]
    }

emptySamples :: Samples
emptySamples = Samples [] [] []
