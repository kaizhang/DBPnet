{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module DBPnet.ReadCount
    ( readCount
    ) where

import Bio.ChIPSeq
import Bio.Data.Bed
import Bio.Utils.Misc (readDouble, readInt)

import Control.Monad (forM, forM_)
import Control.Monad.Base (liftBase)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Lens (makeFields, (^.), (.~))
import qualified Data.Vector.Unboxed as U
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.Default
import qualified Data.Conduit.Binary as Bin
import qualified Data.Conduit.Zlib as Bin
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.List
import Data.Double.Conversion.ByteString (toFixed)
import Statistics.Sample
import Statistics.Distribution (complCumulative)
import Statistics.Distribution.Poisson (poisson)
import System.IO
import Shelly hiding (FilePath, withTmpDir)

import DBPnet.Type
import DBPnet.Utils

data ReadCountOpt = ReadCountOpt
    { readCountOptBinSize :: !Int
    , readCountOptPValue :: !Double
    , readCountOptChromSize :: ![(B.ByteString, Int)]
    , readCountOptVarFilter :: !Double   -- ^ remove constant signal
    } deriving (Show, Read)

makeFields ''ReadCountOpt

instance Default ReadCountOpt where
    def = ReadCountOpt
        { readCountOptBinSize = 1000
        , readCountOptPValue = 1e-2
        , readCountOptChromSize = hg19ChrSize
        , readCountOptVarFilter  = 0.1
        }

hg19ChrSize :: [(B.ByteString, Int)]
hg19ChrSize = [ ("chr1", 249250621)
              , ("chr2", 243199373)
              , ("chr3", 198022430)
              , ("chr4", 191154276)
              , ("chr5", 180915260)
              , ("chr6", 171115067)
              , ("chr7", 159138663)
              , ("chrX", 155270560)
              , ("chr8", 146364022)
              , ("chr9", 141213431)
              , ("chr10", 135534747)
              , ("chr11", 135006516)
              , ("chr12", 133851895)
              , ("chr13", 115169878)
              , ("chr14", 107349540)
              , ("chr15", 102531392)
              , ("chr16", 90354753)
              , ("chr17", 81195210)
              , ("chr18", 78077248)
              , ("chr20", 63025520)
              , ("chr19", 59128983)
              , ("chr22", 51304566)
              , ("chr21", 48129895)
              ]

readCount :: [Experiment] -> FilePath -> ReadCountOpt -> IO [(String, FilePath)]
readCount es outDir opt = withTmpDir outDir $ \tmp -> do
    -- count reads for each experiment
    rs <- forM es $ \e -> do
        let fls = e^.files
            targetName = e^.eid
            output = tmp ++ "/" ++ targetName
        shelly $ mkdir_p $ fromText $ T.pack output
        rc fls output
        return (targetName, output)

    combineAndFilter rs outDir
  where
    rc inputs output = do
        readcounts <- forM inputs $ \input -> do
            let fl = input^.location
                fileFormat = input^.format
            case fileFormat of
                Bed -> readBed fl $$ profiling (opt^.binSize) regions
                BedGZip -> runResourceT $ Bin.sourceFile fl $= Bin.ungzip $=
                           Bin.lines $= CL.map fromLine $$
                           hoist liftBase (profiling (opt^.binSize) regions)
                _ -> undefined

        forM_ (zip (opt^.chromSize) $ merge readcounts) $ \((chr,_), v) -> do
            let outFile = output ++ "/" ++ B.unpack chr
                l = U.length v
                bg_glob = estimateBG v
            withFile outFile WriteMode $ \handle ->
                forM_ [0..l-1] $ \i -> do
                    let getBG x = let len = x `div` 2
                                      vec = U.map (v U.!) $ U.fromList $
                                            filter (\x -> x >= 0 && x < l) $
                                            [i-len..i-1] ++ [i+1,i+len]
                                  in estimateBG vec
                        bg_local = maximum $ bg_glob : map getBG [14, 24]
                        c = v U.! i
                    case () of
                        _ | c == 0 -> B.hPutStrLn handle "0"
                          | complCumulative (poisson bg_local) c <= (opt^.pValue) ->
                                B.hPutStrLn handle $ toFixed 4 $ c / bg_local
                          | otherwise -> B.hPutStrLn handle "0"
      where
        regions = map (\(chr,s) -> BED3 chr 0 s) $ opt^.chromSize
        estimateBG xs = let (m, var) = meanVarianceUnb xs
                        in mean $ U.filter (< (m + 2 * sqrt var)) xs
        merge rs = map f $ transpose counts
          where
            f = foldl1' (\acc x -> U.zipWith (+) acc x)
            counts = flip map rs $ \(values, _) ->
                     map (U.map fromIntegral) values
--            n = fromIntegral $ length rs

    combineAndFilter xs output = do
        shelly $ mkdir_p $ fromText $ T.pack output
        let (ids, dirs)  = unzip xs

        dat <- forM (opt^.chromSize) $ \(chr', _) -> do
            let chr = B.unpack chr'
            rs <- forM dirs $ \dir -> do
                c <- B.readFile $ dir ++ "/" ++ chr
                return $ U.fromList $ map readDouble $ B.lines c
            let (idx, rc) = unzip $ filter (highCV . snd) $
                            zip [0, opt^.binSize ..] $
                            MU.toRows $ MU.fromColumns rs
            return (zip (repeat chr') idx, rc)

        let idx = concat $ fst $ unzip dat
            dat' = MU.fromColumns $ concat $ snd $ unzip dat
        forM (zip ids $ MU.toRows dat') $ \(i, v) -> do
            let outFile = output ++ "/" ++ i ++ ".bed"
            writeBed' outFile $ zipWith toBed idx $ U.toList v
            return (i, outFile)
      where
        highCV v = let (_,var) = meanVarianceUnb v in sqrt var >= opt^.varFilter
        toBed (chr,i) x = BED chr i (i+(opt^.binSize)) Nothing (Just x) Nothing
