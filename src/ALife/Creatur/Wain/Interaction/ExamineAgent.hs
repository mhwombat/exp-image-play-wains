------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Interaction.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Main where

import  ALife.Creatur.Wain.Interaction.Wain
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.Response
import qualified ALife.Creatur.Wain.Scenario as Scenario
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Interaction.Action
import ALife.Creatur.Wain.Interaction.Experiment
import ALife.Creatur.Wain.Interaction.Universe
import ALife.Creatur.Wain.PlusMinusOne
import ALife.Creatur.Wain.UnitInterval
import Control.Lens
import Control.Monad.State
import Data.Map.Strict (elems, toList)
import System.Environment
import Text.Printf (printf)

getAndExamineAll :: StateT (Universe ImageWain) IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names

getAndExamine :: String -> StateT (Universe ImageWain) IO ()
getAndExamine s = do
  a <- getAgent s
  case a of
    (Right agent) -> liftIO $ examine agent
    (Left msg)    -> liftIO $ putStrLn msg

examine :: ImageWain -> IO ()
examine a = do
  putStrLn $ "name: " ++ show (view name a)
  -- appearance
  -- brain
  putStrLn $ "devotion: " ++ printf "%5.3f" (uiToDouble $ view devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (view ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (view passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (uiToDouble $ view energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (uiToDouble $ view passion a)
  putStrLn $ "age: " ++ show (view age a)
  putStrLn $ "total # children borne: "
    ++ show (view childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ show (view childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length . view litter $ a)
  putStrLn $ "counts=" ++ show (elems . counterMap . view classifier . view brain $ a)
  putStrLn $ "size: " ++ show (view wainSize a)
  putStrLn $ "SQ: " ++ show (schemaQuality . view predictor . view brain $ a)
  putStrLn $ "Number of classifier models: " ++ show (numModels . view classifier . view brain $ a)
  putStrLn $ "Classifier learning function " ++ show (view exponentialParams . view classifier . view brain $ a)
  putStrLn $ "Number of predictor models: " ++ show (numModels . view predictor . view brain $ a)
  putStrLn $ "Predictor learning function " ++ show (view exponentialParams . view predictor . view brain $ a)
  -- putStrLn "------------------------"
  -- putStrLn "Mental models of vectors"
  -- putStrLn "------------------------"
  -- mapM_ putStrLn $ concatMap (prettyAudioPattern 9) (toList . classifier . brain $ a)
  putStrLn "-----------------"
  putStrLn "Response models"
  putStrLn "-----------------"
  mapM_ putStrLn $ concatMap prettyResponseModel (toList . modelMap . view predictor . view brain $ a)
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

prettyResponseModel :: (Label, Response Action) -> [String]
prettyResponseModel (l, r) =
  [ "Model " ++ show l,
    "Labels: " ++ show (view (scenario . Scenario.labels) $ r),
    "Energy: " ++ show (head . view (scenario . Scenario.condition) $ r),
    "Passion: " ++ show ((!!2) . view (scenario . Scenario.condition) $ r),
    "Boredom: " ++ show ((!!3) . view (scenario . Scenario.condition) $ r),
    "Action: " ++ show (view action r),
    "Expected happiness change: "
      ++ (printf "%.3g" . pm1ToDouble) (view outcome r),
    "-----" ]

formatVector :: String -> [Double] -> String
formatVector fmt = unwords . map (printf fmt)

main :: IO ()
main = do
  u <- loadUniverse
  t <- evalStateT currentTime u
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT getAndExamineAll u
    else do
      let s = head args
      evalStateT (getAndExamine s) u
