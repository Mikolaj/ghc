module Main where

import GHC hiding (flags, ModuleName)
import qualified Config as GHC
import ErrUtils   ( MsgDoc )
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import System.Process
import Data.Time
import Data.IORef
import Control.Applicative
import qualified Control.Exception as Ex

main :: IO ()
main =
  handleOtherErrors $ do

    libdir <- getGhcLibdir

    runGhc (Just libdir) $
      handleSourceError printException $ do

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 $ [noLoc "-XCPP"]

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscNothing,
                             ghcLink    = NoLink,
                             ghcMode    = CompManager,
                             log_action = collectSrcError,
                             verbosity  = 1
                           }
        addTarget Target
                { targetId           = TargetFile "Ticks.hs" Nothing
                , targetAllowObjCode = True
                , targetContents     = Nothing
                }
        load LoadAllTargets
        return ()

    return ()
  where
    handleOtherErrors =
      Ex.handle $ \e ->
        putStrLn $ "Exception:\n" ++ show (e :: Ex.SomeException) ++ "\n"

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

collectSrcError :: DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError flags severity srcspan style msg = do
  let showSeverity SevOutput  = "SevOutput"
      showSeverity SevDump    = "SevDump"
      showSeverity SevInfo    = "SevInfo"
      showSeverity SevWarning = "SevWarning"
      showSeverity SevError   = "SevError"
      showSeverity SevFatal   = "SevFatal"
  putStrLn
    $  "Normal error message:\nSeverity: "   ++ showSeverity severity
    ++ "  SrcSpan: "  ++ show srcspan
--    ++ "  PprStyle: " ++ show style
    ++ "  MsgDoc: "   ++ showSDocForUser flags (qualName style,qualModule style) msg
    ++ "\n"
