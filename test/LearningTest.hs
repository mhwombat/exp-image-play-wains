------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can be taught.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Interaction.Action (Action(..))
import ALife.Creatur.Wain.Interaction.Experiment
import ALife.Creatur.Wain.Interaction.Image
import ALife.Creatur.Wain.Interaction.FMRI
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.Response (action, outcome)
import ALife.Creatur.Wain.GeneticSOMInternal (ExponentialParams(..),
  modelMap)
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, scenarioReport, responseReport, decisionReport,
  decisionQuality)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Ord (comparing)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (view)
import System.Directory

interactionDeltaE :: [Double]
interactionDeltaE=[1, 0.8, 0.6, 0.3, 0.2, 0.1, 0, -0.1, -0.2, -0.3, -0.05]

interactionDeltaB :: [Double]
interactionDeltaB=[0, -0.1, -0.2, -0.3, -0.6, -0.8, -1.0, -0.2, 0, 0, 0.1]

runAction :: Action -> Object -> ImageWain -> ImageWain
runAction Eat obj w = w'
  where (w', _, _) = adjustEnergy (interactionDeltaE !! objectNum obj) w
runAction Play obj w = w'
  where (w', _) = adjustBoredom (interactionDeltaB !! objectNum obj) w
runAction Flirt obj w =
  if isImage obj
    then w'
    else error "mating not supported"
  where (w', _, _) = adjustEnergy (-0.001) w
runAction Ignore _ w = w

testWain :: ImageWain
testWain = w'
  where wName = "Fred"
        wAppearance = bigX 28 28
        wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec n 0.13 ImageTweaker
        n = 25
        wMuser = makeMuser 0 2
        wPredictor = buildPredictor ep (n*4) 0.1 cw rw
        wHappinessWeights = makeWeights [1, 0, 0]
        ec = ExponentialParams 0.2 0.0001
        ep = ExponentialParams 0.1 0.0001
        cw = makeWeights [1, 0, 0]
        rw = makeWeights [0.9, 0.1]
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _, _) = adjustEnergy 0.5 w

tryOne :: ImageWain -> Object -> IO (ImageWain)
tryOne w obj = do
  putStrLn $ "-----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  describeClassifierModels w
  putStrLn "Initial decision models"
  describePredictorModels w
  let (lds, sps, rplos, aos, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _) = minimumBy (comparing snd) . head $ lds
  mapM_ putStrLn $ scenarioReport sps
  mapM_ putStrLn $ responseReport rplos
  mapM_ putStrLn $ decisionReport aos
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  -- describeClassifierModels wainAfterDecision
  -- describePredictorModels wainAfterDecision
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcome " ++ show (view outcome r)
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putStrLn $ "Δh=" ++ show deltaH
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if deltaH < 0
    then putStrLn " was wrong"
    else putStrLn " was correct"
  let (wainAfterReflection, err) = reflect [objectAppearance obj] r wainRewarded
  putStrLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  -- keep the wain's boredom constant
  let restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
  let (wainPartiallyRestored, _, _) = adjustEnergy restorationEnergy wainAfterReflection
  let (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored
  -- putStrLn "Final classifier models"
  -- describeClassifierModels wainFinal
  -- putStrLn "Final decision models"
  -- describePredictorModels wainFinal
  putStrLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putStrLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putStrLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

describeClassifierModels :: ImageWain -> IO ()
describeClassifierModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, r) = view name w ++ "'s classifier model "
                     ++ show l ++ ": <img src='data:image/png;base64,"
                     ++ base64encode r ++ "'/>"

describePredictorModels :: ImageWain -> IO ()
describePredictorModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ ": "
                     ++ pretty r

dir :: String
dir = "/home/eamybut/mnist/testData/"

readImage2 :: FilePath -> IO Object
readImage2 f = do
  img <- readImage (dir ++ f)
  return $ IObject img f

main :: IO ()
main = do
  files <- take 1000 . drop 2 <$> getDirectoryContents dir
  imgs <- mapM readImage2 files
  w <- foldM tryOne testWain imgs
  putStrLn "test complete"
  let ss = mkSizeSpec2D (Just 500) Nothing
  let diagram = drawClassifier . M.toList . modelMap . view classifier . view brain $ w :: QDiagram SVG V2 Double Any
  let outputFileName = "learn.svg"
  renderSVG outputFileName ss diagram

