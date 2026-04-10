module Paths_ (
    getDataDir,
    getDataFileName,
) where

import System.FilePath ((</>))
import System.Environment (lookupEnv)

getDataFileName
    :: String       -- ^ Package name
    -> FilePath     -- ^ filename
    -> IO FilePath
getDataFileName pn path = fmap (</> path) (getDataDir pn)

getDataDir :: String -> IO FilePath
getDataDir pn = do
    -- @cabal-install@ sets `<pkgname>_datadir` variable,
    -- that is what true Paths_ modules look for as well.
    mpath <- lookupEnv (pn ++ "_datadir")
    case mpath of
        Just path -> return path
        -- if environment variable is not set, use current directory
        -- TODO: maybe look around for `<pkgname>.cabal`
        Nothing -> return "."
