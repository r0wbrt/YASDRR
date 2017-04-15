{-

Copyright 2017 Robert Christian Taylor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

{- |
Module      :  Main
Description :  Yasdrr main entry point
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable

-}

-- | System imports
import qualified Control.Monad         as CM
import           System.Console.GetOpt as GetOpt
import           System.Environment
import           System.Exit
import           System.IO

-- Yasdrr shared imports
import qualified Shared.ChirpRx        as ChirpRx
import qualified Shared.ChirpTx        as ChirpTx
import qualified Shared.CommandLine    as CL
import qualified Shared.MorseTx        as MorseTx


-- | Record that represents the options of yasdrr
data ProgramOptions = ProgramOptions
 { optExecutionMode         :: CL.ExecutionMode
 , optHelpMessageRequested  :: Bool
 , optAboutMessageRequested :: Bool
 }


 -- | Default options for yasdrr executable
startOptions :: ProgramOptions
startOptions = ProgramOptions
 { optExecutionMode = CL.ExecutionModeNotSet
 , optHelpMessageRequested = False
 , optAboutMessageRequested = False
 }


 -- | Starting command line options for the yasdrr executable
commandLineOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
commandLineOptions =
 [ CL.inputMode (\mode opt -> return $ opt { optExecutionMode = mode })
 , CL.inputAbout (\opt -> return $ opt { optAboutMessageRequested = True })
 , CL.inputHelp (\opt -> return $ opt {optHelpMessageRequested = True })
 ]


-- | Main entry point for the yasdrr program
main :: IO ()
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case GetOpt.getOpt' GetOpt.Permute commandLineOptions arguments of
         (actions, [], extras, []) -> do
            programOptions <- CL.processInput startOptions actions

            let finalCommandInput = makeFinalCommandInputString extras (optHelpMessageRequested programOptions) (optAboutMessageRequested programOptions)


            _ <- case optExecutionMode programOptions of
                 CL.ExecutionModeNotSet -> do
                     CM.when (optHelpMessageRequested programOptions) mainHelp
                     CM.when (optAboutMessageRequested programOptions) mainAbout
                     hPutStrLn stderr "Mode Must be specified"
                     exitFailure
                 CL.ChirpReceive ->
                     ChirpRx.chirpRxMainIO finalCommandInput
                 CL.ChirpTransmit ->
                     ChirpTx.chirpTxMainIO finalCommandInput
                 CL.MorseTransmit ->
                     MorseTx.morseTxMainIO finalCommandInput
                 _ -> do
                     hPutStrLn stderr "Execution mode supplied was invalid"
                     exitFailure


            exitSuccess
         (_, _, _, errors) -> do
            hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
            exitFailure


-- | Adds back the help and about string for child programs
makeFinalCommandInputString :: [String] -> Bool -> Bool -> [String]
makeFinalCommandInputString options True True = options ++ ["--help", "--about"]
makeFinalCommandInputString options True False = options ++ ["--help"]
makeFinalCommandInputString options False True = options ++ ["--about"]
makeFinalCommandInputString options False False = options


-- | Generates a help message for the base yasdrr program.
mainHelp :: IO ()
mainHelp = do
    _ <- CL.commonHelpHandler commandLineOptions Nothing startOptions
    return ()


-- | Generates the about message for the base yasdrr program
mainAbout :: IO ()
mainAbout = do
    _ <- CL.commonAboutHandler commandLineOptions Nothing "" startOptions
    return ()
