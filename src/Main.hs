{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens((.~), (^.))
import Control.Arrow (second)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Paths_DBPnet (version)
import Data.Version (showVersion)
import Text.Printf (printf)
import Options.Applicative
import Data.Default
import Shelly hiding (FilePath, withTmpDir)
import qualified Data.Text as T
import qualified Data.Matrix.Unboxed as MU

import DBPnet.ReadCount
import DBPnet.Correlation
import DBPnet.Network
import DBPnet.Utils
import DBPnet.Type (Experiment)
import DBPnet.Constants

data Options = Options
    { input :: FilePath
    , output :: FilePath
    , loop :: Maybe FilePath
    , lambda :: Double
    , chrSize :: String
    } deriving (Show, Read)

parser :: Parser Options
parser = Options
     <$> argument str (metavar "INPUT")
     <*> strOption
           ( long "output"
          <> short 'o'
          <> value "DBPnet_output"
          <> metavar "OUTPUT" )
     <*> (optional . strOption)
           ( long "loop"
          <> short 'l'
          <> metavar "LOOP_FILE"
          <> help "a file providing chromosome long range interactions" )
     <*> option auto
           ( short 'r'
          <> metavar "LAMBDA"
          <> value 0.3
          <> help "cutoff used in glasso algorithm, default: 0.3" )
     <*> strOption
           ( long "chrom_size"
          <> short 'c'
          <> metavar "CHROM_SIZE" )

defaultMain :: Options -> IO ()
defaultMain (Options inFl outDir lp cutoff chrsize) = do
    chr <- case chrsize of
        "hg19" -> return hg19ChrSize
        "mm10" -> return mm10ChrSize
        _ -> readChrSize chrsize

    r <- decodeFileEither inFl
    case r of
        Left e -> error $ prettyPrintParseException e
        Right input -> do
            let output2D = outDir ++ "/Network_2D/"
                output3D = outDir ++ "/Network_3D/"
                output2D3D = outDir ++ "/Network_2D_Plus_3D/"
            shelly $ mkdir_p $ fromText $ T.pack output2D
            shelly $ mkdir_p $ fromText $ T.pack output3D
            shelly $ mkdir_p $ fromText $ T.pack output2D3D
            withTmpDir outDir $ \tmp -> do
                rc <- readCount input tmp $ chromSize .~ chr $ def
                cis <- second zeroNeg <$> cisCorMat rc

                -- Output network built with 2D correlation only
                outputResults output2D (fst cis) (buildNetwork (snd cis) cutoff)
                    ("2D_correlation", snd cis) Nothing input

                case lp of
                    Just l -> do
                        t <- second zeroNeg <$> transCorMat rc l
                        -- Output network built with 3D correlation only
                        outputResults output3D (fst t) (buildNetwork (snd t) cutoff)
                            ("3D_correlation", snd t) Nothing input

                        let hybrid = cisTransCombine cis t
                        -- Output network built with 2d and 3d correlation
                        outputResults output2D3D (fst hybrid)
                            (buildNetwork (snd hybrid) cutoff)
                            ("2D_correlation", snd cis)
                            (Just ("3D_correlation", snd t)) input
                    Nothing -> return ()
  where
    zeroNeg = MU.map (\x -> if x < 0 then 0 else x)

main :: IO ()
main = execParser opts >>= defaultMain
  where
    opts = info (helper <*> parser)
            ( fullDesc
           <> header (printf "DBPnet-v%s" (showVersion version)) )
