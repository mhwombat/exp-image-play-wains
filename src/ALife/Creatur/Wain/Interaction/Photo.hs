------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Interaction.Photo
-- Copyright   :  (c) Amy de Buitléir 2014-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import  ALife.Creatur.Wain.Interaction.Wain
import ALife.Creatur.Wain.Interaction.Experiment
import ALife.Creatur.Wain.Interaction.Image
import ALife.Creatur.Wain.Interaction.Universe
import Control.Lens hiding ((#), none)
import Control.Monad.State
import Data.Colour.SRGB
import Data.Typeable
import Data.Word
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (view)
import System.Environment

grey2colour :: Word8 -> Colour Double
grey2colour x = sRGB x' x' x'
  where x' = fromIntegral x / 255

colour2square
  :: (Typeable (N b), TrailLike b, HasStyle b, V b ~ V2)
    => Colour Double -> b
colour2square c = square 0.1 # fc c # lw none

imageRow
  :: (Typeable (N c), Monoid c, TrailLike c, Semigroup c, HasOrigin c,
    Juxtaposable c, HasStyle c, V c ~ V2)
       => [Word8] -> c
imageRow = hcat . map (colour2square . grey2colour)

image2diagram
  :: (Typeable (N c), Monoid c, TrailLike c, Semigroup c, HasOrigin c,
    Juxtaposable c, HasStyle c, V c ~ V2)
     => Image -> c
image2diagram = vcat . map imageRow . pixelArray

cream :: (Ord b, Floating b) => Colour b
cream = sRGB24 255 255 224

getWain :: String -> StateT (Universe ImageWain) IO ImageWain
getWain s = do
  a <- getAgent s
  case a of
    (Right agent) -> return agent
    (Left msg)    -> error msg 
  
getWainName :: IO String
getWainName = do
  args <- getArgs
  if null args
    then error "Need to supply a wain name!"
    else return (head args)

main :: IO ()
main = do
  u <- loadUniverse
  n <- getWainName
  w <- evalStateT (getWain n) u
  let ss = mkSizeSpec2D (Just 500) Nothing
  let diagram = image2diagram . view appearance $ w :: QDiagram SVG V2 Double Any
  let outputFileName = n ++ ".svg"
  renderSVG outputFileName ss diagram
