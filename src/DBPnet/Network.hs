{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module DBPnet.Network
    ( cisTransCombine
    , buildNetwork
    , outputResults
    ) where

import           Algorithms.GLasso

import           Bio.Utils.Misc
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
import           Data.List
import           Data.List.Split       (chunksOf)
import qualified Data.Matrix.Unboxed   as MU
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Vector           as V

import           DBPnet.Type

cisTransCombine :: ([B.ByteString], MU.Matrix Double)
                -> ([B.ByteString], MU.Matrix Double)
                -> ([B.ByteString], MU.Matrix Double)
cisTransCombine (h1, cisCor) (h2, transCor) =
    if h1 /= h2
       then error "header mismatched when combining cis and trans"
       else (h1, MU.zipWith max' cisCor transCor)
  where
    max' a b = let x = max a b
               in if x < 0 then 0 else x

buildNetwork :: MU.Matrix Double
             -> Double
             -> [[Int]]
buildNetwork mat cutoff =
    let n = MU.cols mat
        icov = chunksOf n $ snd $ glasso' n (concat $ MU.toLists mat) cutoff
        adjMat = (map.map) (\x -> if x /= 0 then 1 else 0) icov
    in adjMat
  where
    max' (a,b) | abs a > abs b = a
               | otherwise = b

outputResults :: FilePath
              -> [B.ByteString]    -- ^ ids
              -> [[Int]]
              -> (B.ByteString, MU.Matrix Double)
              -> Maybe (B.ByteString, MU.Matrix Double)
              -> [Experiment] -> IO ()
outputResults dir ids adj (mat1Name, mat1) mat2' es = do
    let idToName = M.fromList $ map (\x -> (B.pack $ x^.eid, B.pack $ x^.target)) es
        names = map (\i -> M.lookupDefault undefined i idToName) ids
        header = B.intercalate "\t" names
        adj' = MU.fromLists adj :: MU.Matrix Int
        n = MU.cols adj'
        edges = filter ((/=0) . (adj' MU.!)) $ [ (i,j) | i <- [0..n-1], j <- [i+1..n-1] ]
        edgesNames = map (\(i,j) -> B.intercalate "<->" [names !! i, names !! j]) edges
        score1 = map (mat1 MU.!) edges
        mat2 = case mat2' of
            Just (mat2Name, mat2) -> Just (mat2Name, map (mat2 MU.!) edges)
            Nothing -> Nothing

    -- output adjacency matrix
    B.writeFile (dir ++ "/adjMatrix.tsv") $ B.unlines $
        header : map (B.intercalate "\t" . map (B.pack . show)) adj

    -- output edges
    let es = transpose $ case mat2 of
            Nothing -> ("Edges" : edgesNames)
                     : [mat1Name : map (B.pack . show) score1]
            Just (mat2Name, score2) -> ("Edges" : edgesNames)
                     : (mat1Name : map (B.pack . show) score1)
                     : [mat2Name : map (B.pack . show) score2]
    B.writeFile (dir ++ "/edges.tsv") $ B.unlines $ map (B.intercalate "\t") es
