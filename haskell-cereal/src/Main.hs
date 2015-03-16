{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Monad       (replicateM)
import           Data.Monoid ((<>))
import           Criterion.Main      (bench, bgroup, defaultMain, env, whnf)
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy     as LB
import           Data.Binary.Get hiding (remaining, label)
import           Data.Word

data LV = LV !Int !BOX deriving Show
data BOX = FTYP !B.ByteString !Word32 ![B.ByteString] | FREE | MOOV | SKIP | MDAT deriving Show

ftypBox :: Int -> Get BOX
ftypBox l = do
  major_brand         <- getBytes 4
  major_brand_version <- getWord32be
  let remaining       = (l - 8) `div` 4
  compatible_brands   <- replicateM remaining (getBytes 4)
  return $ FTYP major_brand major_brand_version compatible_brands

mp4Box :: Get LV
mp4Box = do
  length <- fmap fromIntegral getWord32be
  let remaining = length - 4
  bs <- getLazyByteString (fromIntegral remaining)
  let !value = runGet (anyBox remaining) bs
  return (LV length value)

anyBox :: Int -> Get BOX
anyBox l = do label <- getBytes 4
              case label of
                "ftyp"  -> ftypBox (l - 4)
                "free"  -> return FREE
                "mdat"  -> return MDAT
                "moov"  -> return MOOV
                "skip"  -> return SKIP
                boxType -> error (show ("Type not recognized, was " <> boxType))

mp4Parser :: Get [LV]
mp4Parser = do
  empty <- isEmpty
  if empty
    then return []
    else do trade <- mp4Box
            trades <- mp4Parser
            return (trade:trades)

smallFile :: FilePath
smallFile = "../small.mp4"
bunnyFile :: FilePath
bunnyFile = "../bigbuckbunny.mp4"

setupEnv :: IO (LB.ByteString, LB.ByteString)
setupEnv = do
  small <- LB.readFile smallFile
  bunny <- LB.readFile bunnyFile
  return (small, bunny)

criterion :: IO ()
criterion = defaultMain
    [
      env setupEnv $ \ ~(small,bunny) ->
      bgroup "IO"
        [
          bench "small"          $ whnf (runGet mp4Parser) small
        , bench "big buck bunny" $ whnf (runGet mp4Parser) bunny
        ]
    ]

main :: IO ()
main = criterion
-- main = do LB.readFile smallFile >>= print . runGet mp4Parser
--           LB.readFile bunnyFile >>= print . runGet mp4Parser
