{-# LANGUAGE OverloadedStrings #-}

module DBPnet.Constants
    ( hg38ChrSize
    , hg19ChrSize
    , mm10ChrSize
    ) where

import qualified Data.ByteString.Char8 as B

hg38ChrSize :: [(B.ByteString, Int)]
hg38ChrSize = [ ("chr1", 248956422)
              , ("chr2", 242193529)
              , ("chr3", 198295559)
              , ("chr4", 190214555)
              , ("chr5", 181538259)
              , ("chr6", 170805979)
              , ("chr7", 159345973)
              , ("chr8", 145138636)
              , ("chr9", 138394717)
              , ("chr10", 133797422)
              , ("chr11", 135086622)
              , ("chr12", 133275309)
              , ("chr13", 114364328)
              , ("chr14", 107043718)
              , ("chr15", 101991189)
              , ("chr16", 90338345)
              , ("chr17", 83257441)
              , ("chr18", 80373285)
              , ("chr19", 58617616)
              , ("chr20", 64444167)
              , ("chr21", 46709983)
              , ("chr22", 50818468)
              , ("chrX", 156040895)
              , ("chrY", 57227415)
              , ("chrM", 16569) ]

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

mm10ChrSize :: [(B.ByteString, Int)]
mm10ChrSize = [ ("chr1", 195471971)
              , ("chr2", 182113224)
              , ("chrX", 171031299)
              , ("chr3", 160039680)
              , ("chr4", 156508116)
              , ("chr5", 151834684)
              , ("chr6", 149736546)
              , ("chr7", 145441459)
              , ("chr10", 130694993)
              , ("chr8", 129401213)
              , ("chr14", 124902244)
              , ("chr9", 124595110)
              , ("chr11", 122082543)
              , ("chr13", 120421639)
              , ("chr12", 120129022)
              , ("chr15", 104043685)
              , ("chr16", 98207768)
              , ("chr17", 94987271)
              , ("chrY", 91744698)
              , ("chr18", 90702639)
              , ("chr19", 61431566)
              ]
