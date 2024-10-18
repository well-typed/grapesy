module Test.Stress.Driver
  ( driver
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Catch (ExitCase(..), generalBracket)
import Data.IORef
import System.Environment
import System.Exit
import System.Process
import System.Random

import Test.Stress.Common
import Test.Stress.Driver.Summary

-------------------------------------------------------------------------------
-- Top-level
-------------------------------------------------------------------------------

-- | Run the automatic stress test suite
--
-- See documentation in the source repository for details.
driver :: Bool -> Bool -> Maybe FilePath -> Int -> IO ()
driver v genCharts mwd duration = do
    putStrLn $
      "Running stress test driver for " ++ show duration ++ " seconds..."
    exitCodeRef <- newIORef ExitSuccess
    bracket
      ( do
          runningServers <- mapM (forkComponent v genCharts mwd) servers
          runningClients <- mapM (forkComponent v genCharts mwd) clients
          return $ runningServers ++ runningClients
      )
      (\running -> do
        say v "(driver) stopping all components"
        cancelMany running
        say v "(driver) stopped all components"
      )
      (\running -> do
        result <- race (threadDelay (duration * 1_000_000)) (waitAnyCatch running)
        case result of
          Right (_, Left e)
            | Just (TestFailure _) <- fromException e -> do
              writeIORef exitCodeRef (ExitFailure 1)
              throwIO e
            | otherwise ->
              return ()
          _ -> return ()
      )
      `catch`
        \case
          e | Just f@TestFailure{} <- fromException e -> do
                putStrLn $ "stress test failed: " ++ show f
            | otherwise -> do
                say v $ "(driver) exiting cleanly, got exception: " ++ show e

    -- At this point, the heap profiles should all be written in the working
    -- directory. We convert them each to charts of total heap usage over time
    -- and combine into a summary document
    when genCharts $
      createSummaryPlots v mwd
    putStrLn "Done"
    exitCode <- readIORef exitCodeRef
    exitWith exitCode

-------------------------------------------------------------------------------
-- Auxiliary
-------------------------------------------------------------------------------

data Component = Component {
      -- | Client or server?
      componentType   :: ClientServer

      -- | What port to bind/connect to
    , componentPort   :: Int

      -- | Use TLS?
    , componentSecure :: Bool

      -- | Should the component stay running indefinitely?
      --
      -- If 'False', component will be killed and restarted at random intervals
      -- and it will not be configured to write a heap profile.
    , componentStable :: Bool

      -- | Heap size limit in Megabytes (e.g. @Just 15@ becomes @-M15m@)
      --
      -- Set to 'Nothing' for no limit.
    , componentLimit  :: Maybe Int

      -- | Name
      --
      -- Determines the name of the heap profile file that will be written out.
    , componentName   :: String
    }
  deriving (Show)

data ClientServer =
      Client {
          -- | Which compression should be used?
          --
          -- We don't currently use this in the driver to avoid running too many
          -- clients, but it is left here as an option.
          clientCompr :: Maybe String

          -- | Specify which calls should be executed
        , clientFlags :: [String]
        }
    | Server
  deriving (Show, Eq)

newtype TestFailure = TestFailure String
  deriving (Show)
  deriving anyclass Exception

cmd :: Bool -> ClientServer -> [String]
cmd v t = mconcat [
      [ "-v" | v ]
    , case t of
        Client mcompr flags ->
          mconcat [
              [ "client" ]
            , [ "--"++compr | Just compr <- [mcompr] ]
            , flags
            ]
        Server ->
          [ "server" ]
    ]

forkComponent :: Bool -> Bool -> Maybe FilePath -> Component -> IO (Async ())
forkComponent v genCharts mwd c = do
    say v $ "(driver) forking component " ++ show c
    async $
      runComponent v genCharts mwd c `catch`
        \case
          (e :: SomeException)
            | Just AsyncCancelled <- fromException e ->
                say v $ "(driver) cancelled forked component " ++ show c
            | otherwise -> do
                say v $ "(driver) component exited with " ++ show e
                throwIO e

runComponent :: Bool -> Bool -> Maybe FilePath -> Component -> IO ()
runComponent v genCharts mwd c@Component{..} = do
    say' $ "starting " ++ componentName
    exe <- getExecutablePath
    let cp = proc exe (mconcat [
            cmd v componentType
          , [ "--port=" ++ show componentPort ]
          , [ "--secure" | componentSecure ]
          , if componentType == Server then
              [ "+RTS", "-N", "-RTS" ]
            else
              []
          , filter (const $ componentStable && genCharts) [
                "+RTS"
              , "-l"
              , "-hT"
              , "-ol" ++ componentName ++ ".eventlog"
              , "-RTS"
              ]
          , case componentLimit of
              Just limit -> [
                  "+RTS"
                , "-M" ++ show limit ++ "m"
                , "-RTS"
                ]
              Nothing ->
                []
          ])
    (k, ec) <-
      generalBracket
        (do
          (_, _, _, ph) <-
            createProcess_
              ("runComponent (" ++ componentName ++ ")")
              cp {cwd = mwd}
          return ph
        )
        (\ph ec -> do
          terminateProcess ph
          return ec
        )
        watchComponent
    case ec of
      ExitCaseSuccess _ -> k
      ExitCaseException e
        | Just (TestFailure _) <- fromException e ->
          throwIO e
        | otherwise ->
          k
      ExitCaseAbort -> throwIO $ TestFailure $ componentName ++ " aborted"
  where
    watchComponent :: ProcessHandle -> IO (IO ())
    watchComponent ph =
        if componentStable then do
          say' $ "watching " ++ componentName
          ec <- waitForProcess ph
          case ec of
            ExitFailure _ -> do
              say' $ "unexpected ExitFailure from " ++ show componentName
              throwIO $ TestFailure componentName
            ExitSuccess -> do
              say' $ componentName ++ " exited successfully, rerunning"
              return $ runComponent v genCharts mwd c
        else do
          d <- randomRIO (16_000_000, 20_000_000)
          say' $
            "waiting " ++ show d ++ "µs before killing " ++ show componentName
          threadDelay d
          mec <- getProcessExitCode ph
          case mec of
            Just (ExitFailure _) -> do
              say' $ "unexpected ExitFailure from " ++ show componentName
              throwIO $ TestFailure componentName
            _ -> do
              say' $ "terminating " ++ show componentName
              terminateProcess ph
              d' <- randomRIO (200_000, 500_000)
              say' $
                "waiting " ++ show d ++ "µs before restarting " ++
                show componentName
              threadDelay d'
              return $ runComponent v genCharts mwd c

    say' :: String -> IO ()
    say' = say v . ("(driver) " ++)

servers :: [Component]
servers = [
      Component {
          componentType   = Server
        , componentPort   = 50000
        , componentSecure = False
        , componentStable = False
        , componentLimit  = Just 60
        , componentName   = "server-unstable-insecure"
        }
    , Component {
          componentType   = Server
        , componentPort   = 50001
        , componentSecure = True
        , componentStable = False
        , componentLimit  = Just 100
        , componentName   = "server-unstable-secure"
        }
    , Component {
          componentType   = Server
        , componentPort   = 50002
        , componentSecure = False
        , componentStable = True
        , componentLimit  = Just 60
        , componentName   = "server-stable-insecure"
        }
    , Component {
          componentType   = Server
        , componentPort   = 50003
        , componentSecure = True
        , componentStable = True
        , componentLimit  = Just 100
        , componentName   = "server-stable-secure"
        }
    ]

clients :: [Component]
clients = [
      Component {
        componentType   = Client Nothing flags
      , componentPort   = snd portSecurity
      , componentSecure = fst portSecurity
      , componentStable = stable
      , componentLimit  = Just 60
      , componentName = mconcat [
            "client-"
          , show (snd portSecurity) ++ "-"
          , if stable then "stable-" else "unstable-"
          , if fst portSecurity then "secure-" else "insecure-"
          , nameStr
          ]
      }
    | (flags, nameStr) <- [
          ( [ indefinitely "--num-connections"
            , "--non-streaming"
            ]
          , "non-streaming-many-connections"
          )
        , ( [ indefinitely "--num-calls"
            , "--non-streaming"
            ]
          , "non-streaming-many-calls"
          )
        , ( [ indefinitely "--client-streaming"
            ]
          , "client-stream"
          )
        , ( [ indefinitely "--num-calls"
            , "--client-streaming=10"
            ]
          , "client-stream-many-calls"
          )
        , ( [ indefinitely "--server-streaming"
            ]
          , "server-stream"
          )
        , ( [ indefinitely "--num-calls"
            , "--server-streaming=10"
            ]
          , "server-stream-many-calls"
          )
        , ( [ indefinitely "--bidi-streaming"
            ]
          , "bidi-stream"
          )
        , ( [ indefinitely "--num-calls"
            , "--bidi-streaming=10"
            ]
          , "bidi-stream-many-calls"
          )
        ]
    , portSecurity <- [
          (False, 50000)
        , (False, 50002)
        , (True , 50001)
        , (True , 50003)
        ]
    , stable <- [True, False]
    ]
  where
    indefinitely :: String -> String
    indefinitely = (++ "=100000000")
