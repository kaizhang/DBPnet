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
            shelly $ mkdir_p $ fromText $ T.pack outDir
            withTmpDir outDir $ \tmp -> do
                rc <- readCount input tmp $ chromSize .~ chr $ def
                cis <- cisCorMat rc
                (dat, trans) <- case lp of
                    Just l -> do
                        t <- transCorMat rc l
                        return (cisTransCombine cis t, Just t)
                    Nothing ->
                        return ( second (MU.map (\x -> if x < 0 then 0 else x)) cis
                               , Nothing )

                outputResults outDir (buildNetwork dat cutoff) cis trans input

main :: IO ()
main = execParser opts >>= defaultMain
  where
    opts = info (helper <*> parser)
            ( fullDesc
           <> header (printf "DBPnet-v%s" (showVersion version)) )
