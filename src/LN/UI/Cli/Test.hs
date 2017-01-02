{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.UI.Cli.Test (
  RunnerReader (..),
  defaultRunnerReader,
  RunnerWriter,
  defaultRunnerWriter,
  RunnerState (..),
  defaultRunnerState,
  defaultApiOpts
) where



import           Control.Monad              (void)
import           Control.Monad.State.Lazy   (get, modify, put)
import           Control.Monad.Trans        (lift)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.RWS    (RWST, runRWST)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Haskell.Api.Helpers        (SpecificApiOptions,
                                             defaultSpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiOptions (..))
import           LN.T.Api                   (ApiResponse)
import           LN.T.Pack.Organization     (OrganizationPackResponse)
import           LN.T.Pack.Sanitized.User   (UserSanitizedPackResponse)



data RunnerReader = RunnerReader {
  rApiOpts :: (ApiOptions SpecificApiOptions)
}

defaultRunnerReader :: RunnerReader
defaultRunnerReader = RunnerReader {
  rApiOpts = defaultApiOpts
}



type RunnerWriter = ()

defaultRunnerWriter :: RunnerWriter
defaultRunnerWriter = ()



data RunnerState = RunnerState {
  blah :: Bool
}



defaultRunnerState :: RunnerState
defaultRunnerState = RunnerState {
  blah = True
}



defaultApiOpts :: ApiOptions SpecificApiOptions
defaultApiOpts = ApiOptions {
  apiUrl         = "http://dev.adarq.org",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Just "x-api-authorization",
  apiOptions     = defaultSpecificApiOptions,
  apiDebug       = False
}



-- runnerM :: forall a. RunnerM a -> IO ()
runApi go = do
  void $ runRWST go defaultRunnerReader defaultRunnerState
  pure ()
