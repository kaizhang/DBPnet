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

buildNetwork :: ([B.ByteString], MU.Matrix Double)
             -> Double
             -> ([B.ByteString], [[Int]])
buildNetwork (names, mat) cutoff =
    let n = MU.cols mat
        icov = chunksOf n $ snd $ glasso' n (concat $ MU.toLists mat) cutoff
        adjMat = (map.map) (\x -> if x /= 0 then 1 else 0) icov
    in (names, adjMat)
  where
    max' (a,b) | abs a > abs b = a
               | otherwise = b

outputResults :: FilePath
              -> ([B.ByteString], [[Int]])
              -> ([B.ByteString], MU.Matrix Double)
              -> Maybe ([B.ByteString], MU.Matrix Double)
              -> [Experiment] -> IO ()
outputResults dir (ids, adj) (_, cis) tr' es = do
    let idToName = M.fromList $ map (\x -> (B.pack $ x^.eid, B.pack $ x^.target)) es
        names = map (\i -> M.lookupDefault undefined i idToName) ids
        header = B.intercalate "\t" names
        adj' = MU.fromLists adj :: MU.Matrix Int
        n = MU.cols adj'
        edges = filter ((/=0) . (adj' MU.!)) $ [ (i,j) | i <- [0..n-1], j <- [i+1..n-1] ]
        edgesNames = map (\(i,j) -> B.intercalate "<->" [names !! i, names !! j]) edges
        cisScores = map (cis MU.!) edges
        transScores = case tr' of
            Just (_, trans) -> Just $ map (trans MU.!) edges
            Nothing -> Nothing

    -- output adjacency matrix
    B.writeFile (dir ++ "/adjMatrix.tsv") $ B.unlines $
        header : map (B.intercalate "\t" . map (B.pack . show)) adj

    -- output edges
    let es = transpose $ case transScores of
            Nothing -> ("Edges" : edgesNames)
                     : ["Cis_cor" : map (B.pack . show) cisScores]
            Just t  -> ("Edges" : edgesNames)
                     : ("Cis_cor" : map (B.pack . show) cisScores)
                     : ["Trans_cor" : map (B.pack . show) t]
    B.writeFile (dir ++ "/edges.tsv") $ B.unlines $ map (B.intercalate "\t") es
