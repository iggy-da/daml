-- Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Tasty resource for starting the HTTP JSON API
module DA.Test.HttpJson
  ( withHttpJson
  , HttpJson(..)
  ) where

import Control.Exception
import DA.Bazel.Runfiles
import DA.PortFile
import System.FilePath
import System.IO.Extra
import System.Process
import Test.Tasty
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Web.JWT as JWT
import qualified Data.Map as Map
import qualified Data.Text as T

getHttpJsonProc :: IO Int -> FilePath -> IO CreateProcess
getHttpJsonProc getLedgerPort portFile = do
  ledgerPort <- getLedgerPort
  jsonApi <-
    locateRunfiles (mainWorkspace </> "ledger-service" </> "http-json" </> exe "http-json-binary")
  pure $
    proc
      jsonApi
      [ "--http-port=0"
      , "--ledger-host"
      , "localhost"
      , "--ledger-port"
      , show ledgerPort
      , "--port-file"
      , portFile
      , "--allow-insecure-tokens"
      ]

createHttpJson :: Handle -> IO Int -> IO HttpJsonResource
createHttpJson httpJsonOutput getLedgerPort = do
  (tmpDir, rmTmpDir) <- newTempDir
  let portFile = tmpDir </> "http-json.portfile"
  let tokenFile = tmpDir </> "http-json.token"
  writeFileUTF8 tokenFile $ T.unpack token
  httpJsonProc <- getHttpJsonProc getLedgerPort portFile
  mask $ \unmask -> do
    ph <- createProcess httpJsonProc {std_out = UseHandle httpJsonOutput}
    let cleanup = cleanupProcess ph >> rmTmpDir
    let waitForStart = do
          port <- readPortFile maxRetries portFile
          pure
            (HttpJsonResource
               { httpJsonProcess = ph
               , httpJsonPort = port
               , httpJsonTokenFile = tokenFile
               , httpJsonDestroy = cleanup
               })
    unmask (waitForStart `onException` cleanup)
  where
    token =
      JWT.encodeSigned
        (JWT.HMACSecret "secret")
        mempty
        mempty
          { JWT.unregisteredClaims =
              JWT.ClaimsMap $
              Map.fromList
                [ ( "https://daml.com/ledger-api"
                  , Object $
                    HashMap.fromList
                      [ ("actAs", toJSON ["Alice" :: T.Text])
                      , ("ledgerId", "MyLedger")
                      , ("applicationId", "foobar")
                      ])
                ]
          }

withHttpJson :: IO Int -> (IO HttpJson -> TestTree) -> TestTree
withHttpJson getLedgerPort f =
  withResource
    (createHttpJson stdout getLedgerPort)
    httpJsonDestroy
    (f . fmap fromHttpJsonResource)

data HttpJsonResource = HttpJsonResource
  { httpJsonProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , httpJsonPort :: Int
  , httpJsonTokenFile :: FilePath
  , httpJsonDestroy :: IO ()
  }

data HttpJson = HttpJson
  { hjPort :: Int
  , hjTokenFile :: FilePath
  }

fromHttpJsonResource :: HttpJsonResource -> HttpJson
fromHttpJsonResource HttpJsonResource {httpJsonPort, httpJsonTokenFile} =
  HttpJson {hjPort = httpJsonPort, hjTokenFile = httpJsonTokenFile}
