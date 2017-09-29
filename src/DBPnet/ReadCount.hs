{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module DBPnet.ReadCount
    ( readCount
    , ReadCountOpt
    , binSize
    , pValue
    , chromSize
    , varFilter
    ) where

import           Bio.ChIPSeq
import           Bio.Data.Bed
import           Bio.Utils.Misc                    (readDouble, readInt)
import           Conduit
import           Control.Lens                      (makeFields, (.~), (^.))
import           Control.Monad                     (forM, forM_)
import           Control.Monad.Base                (liftBase)
import           Control.Monad.Morph               (hoist)
import qualified Data.ByteString.Char8             as B
import qualified Data.Conduit.Zlib                 as Zlib
import           Data.Default
import           Data.Double.Conversion.ByteString (toFixed)
import           Data.Int                          (Int32)
import           Data.List
import qualified Data.Matrix.Unboxed               as MU
import qualified Data.Text                         as T
import qualified Data.Vector.Unboxed               as U
import           Shelly                            hiding (FilePath, withTmpDir)
import           Statistics.Distribution           (complCumulative)
import           Statistics.Distribution.Poisson   (poisson)
import           Statistics.Sample
import           System.IO

import           DBPnet.Type
import           DBPnet.Utils

data ReadCountOpt = ReadCountOpt
    { readCountOptBinSize   :: !Int
    , readCountOptPValue    :: !Double
    , readCountOptChromSize :: ![(B.ByteString, Int)]
    , readCountOptVarFilter :: !Double   -- ^ remove constant signal
    } deriving (Show, Read)

makeFields ''ReadCountOpt

instance Default ReadCountOpt where
    def = ReadCountOpt
        { readCountOptBinSize = 1000
        , readCountOptPValue = 1e-5
        , readCountOptChromSize = []
        , readCountOptVarFilter  = 0.1
        }

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
                Bed -> readBed fl $$ countTagsBinBed (opt^.binSize) regions
                BedGZip -> runResourceT $ sourceFile fl $= Zlib.ungzip $=
                           linesUnboundedAsciiC $= mapC fromLine $$
                           hoist liftBase (countTagsBinBed (opt^.binSize) regions)
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
                        bg_local = maximum $ bg_glob : map getBG [5, 10]
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
                     map (U.map (fromIntegral :: Int32 -> Double)) values
-- we do not need to normalize or average because we don't use input/control
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
