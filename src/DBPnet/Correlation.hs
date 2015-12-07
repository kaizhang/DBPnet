{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DBPnet.Correlation
    ( cisCorMat
    , transCorMat
    ) where

import Control.Monad
import Bio.Data.Bed
import Bio.Utils.Misc
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function (on)
import Data.Ord (comparing)
import Control.Lens
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix.Unboxed.Mutable as MUM
import Statistics.Correlation (spearman)

import DBPnet.Utils (readLoops)

cisCorMat :: [(String, FilePath)]
          -> IO ([B.ByteString], MU.Matrix Double)
cisCorMat dat = do
    counts <- fmap V.fromList $ forM dat $ \(i, fl) -> do
        beds <- readBed' fl
        return (i, U.fromList $ map (fromJust . _score) $ beds)

    let l = V.length counts
        header = map B.pack $ V.toList $ fst . V.unzip $ counts
        mat :: MU.Matrix Double
        mat = MU.create $ do
            m <- MUM.new (l,l)
            forM_ [0..l-1] $ \i -> do
                let (_, tfA) = counts V.! i
                forM_ [0..l-1] $ \j -> case () of
                    _ | j < i -> MUM.read m (j,i) >>= MUM.write m (i,j)
                      | otherwise -> do
                          let (_, tfB) = counts V.! j
                              vec = U.filter (\(a,b) -> a /= 0 || b /= 0) $ U.zip tfA tfB
                              cor | U.length vec < 500 = 0
                                  | otherwise = spearman vec
                          MUM.write m (i,j) cor
            return m
    return (header, mat)

transCorMat :: [(String, FilePath)] -> FilePath
            -> IO ([B.ByteString], MU.Matrix Double)
transCorMat dat hic = do
    loops <- concatMap (\(a,b) -> [a,b]) <$> readLoops hic

    counts <- fmap V.fromList $ forM dat $ \(i,fl) -> do
        beds <- readBed' fl
        let rs = snd $ unzip $ intersectBedWith fn loops beds
            fn [] = 0
            fn x = maximum . map (fromJust . _score) $ x
        return (i, U.fromList $ map (\[a,b] -> (a,b)) $ chunksOf 2 rs)

    let l = V.length counts
        header = map B.pack $ V.toList $ fst . V.unzip $ counts
        mat :: MU.Matrix Double
        mat = MU.create $ do
            m <- MUM.new (l,l)
            forM_ [0..l-1] $ \i -> do
                let (_, tfA) = counts V.! i
                forM_ [0..l-1] $ \j -> case () of
                    _ | j < i -> MUM.read m (j,i) >>= MUM.write m (i,j)
                      | otherwise -> do
                          let (_, tfB) = counts V.! j
                              vec = U.filter (\(a,b) -> a /= 0 || b /= 0) $ U.zipWith f tfA tfB
                              cor | U.length vec < 500 = 0
                                  | otherwise = spearman vec
                          MUM.write m (i,j) cor
            return m
    return (header, mat)
  where
    f (x1,x2) (y1,y2) | x1 > x2 = (x1, y2)
                      | otherwise = (x2, y1)
