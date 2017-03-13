

import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt as GetOpt
import qualified Shared.CommandLine as CL
import qualified Shared.ChirpRx as ChirpRx
import qualified Shared.ChirpTx as ChirpTx
import qualified Shared.MorseTx as MorseTx
import qualified Control.Monad as CM

data ProgramOptions = ProgramOptions 
 { optExecutionMode :: CL.ExecutionMode 
 , optHelpMessageRequested :: Bool
 , optAboutMessageRequested :: Bool
 }

startOptions :: ProgramOptions 
startOptions = ProgramOptions 
 { optExecutionMode = CL.ExecutionModeNotSet
 , optHelpMessageRequested = False
 , optAboutMessageRequested = False
 }

commandLineOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
commandLineOptions = 
 [ CL.inputMode (\mode opt -> return $ opt { optExecutionMode = mode })
 , CL.inputAbout (\opt -> return $ opt { optAboutMessageRequested = True })
 , CL.inputHelp (\opt -> return $ opt {optHelpMessageRequested = True })
 ]

main :: IO ()
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
                 CL.ChirpReceive -> do
                     ChirpRx.chirpRxMainIO finalCommandInput
                 CL.ChirpTransmit -> do
                     ChirpTx.chirpTxMainIO finalCommandInput
                 CL.MorseTransmit -> do
                     MorseTx.morseTxMainIO finalCommandInput
                 _ -> do
                     hPutStrLn stderr "Execution mode supplied was invalid"
                     exitFailure
                 
                     
            exitSuccess
         (_, _, _, errors) -> do
            hPutStrLn stderr $ unlines $ ["Invalid input supplied ~~"] ++ errors
            exitFailure


makeFinalCommandInputString :: [String] -> Bool -> Bool -> [String]
makeFinalCommandInputString options True True = options ++ ["--help", "--about"]
makeFinalCommandInputString options True False = options ++ ["--help"]
makeFinalCommandInputString options False True = options ++ ["--about"]
makeFinalCommandInputString options False False = options

mainHelp :: IO ()
mainHelp = do 
    _ <- CL.commonHelpHandler commandLineOptions Nothing startOptions
    return ()


mainAbout :: IO ()
mainAbout = do 
    _ <- CL.commonAboutHandler commandLineOptions Nothing "" startOptions
    return ()
