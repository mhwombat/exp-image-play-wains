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

import ALife.Creatur.Util (shuffle)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Interaction.Action (Action(..))
import ALife.Creatur.Wain.Interaction.Experiment
import ALife.Creatur.Wain.Image
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Object (Object(..), objectNum, objectId,
  objectAppearance, isImage)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.Response (Response, action, outcomes)
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
import Control.Monad.Random (evalRand, mkStdGen)
import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath.Posix (takeFileName)

interactionDeltaE :: [Double]
interactionDeltaE=[1, 0.8, 0.6, 0.3, 0.2, 0.1, 0, -0.1, -0.2, -0.3, -0.05]

interactionDeltaB :: [Double]
interactionDeltaB=[0, -0.1, -0.2, -0.3, -0.6, -0.8, -1.0, -0.2, 0, 0, 0.1]

runAction :: Action -> Object Action -> ImageWain -> ImageWain
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
        wClassifier = buildClassifier ec n 0.0144 ImageTweaker
        n = 15
        wMuser = makeMuser [0, 0, 0, 0] 1
        wPredictor = buildPredictor ep (n*4) 0.1
        wHappinessWeights = makeWeights [1, 0, 0]
        -- The classifier does most of its learning by round 100.
        ec = ExponentialParams 0.5 0.01
        -- The predictor needs to keep learning longer.
        ep = ExponentialParams 0.1 0.001
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _, _) = adjustEnergy 0.5 w

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

tryOne :: ImageWain -> Object Action -> IO (ImageWain)
tryOne w obj = do
  putHtmlLn $ "-----<br/>"
  putHtmlLn $ "stats=" ++ show (stats w)
  putHtmlLn "Initial classifier models:"
  describeClassifierModels w
  putHtmlLn "Initial prediction models"
  describePredictorModels w
  let (lds, sps, rplos, aos, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _) = minimumBy (comparing snd) . head $ lds
  mapM_ putHtmlLn $ scenarioReport sps
  mapM_ putHtmlLn $ responseReport rplos
  mapM_ putHtmlLn $ decisionReport aos
  putHtmlLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  -- describeClassifierModels wainAfterDecision
  -- describePredictorModels wainAfterDecision
  let a = view action r
  putHtmlLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  putHtmlLn $ "<img src='data:image/png;base64,"
                     ++ base64encode (objectAppearance obj) ++ "'/>"
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putHtmlLn $ "Δh=" ++ show deltaH
  putHtmlLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putHtmlLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putHtml $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if deltaH < 0
    then putHtmlLn " was wrong"
    else putHtmlLn " was correct"
  if interactionDeltaE !! objectNum obj >= 0
    then putHtmlLn $ "Edible - " ++ show a
    else putHtmlLn $ "Poisonous - " ++ show a
  let (wainAfterReflection, err) = reflect [objectAppearance obj] r w wainRewarded
  putHtmlLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  -- keep the wain's boredom constant
  let restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
  let (wainPartiallyRestored, _, _) = adjustEnergy restorationEnergy wainAfterReflection
  let (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored
  -- putHtmlLn "Final classifier models"
  -- describeClassifierModels wainFinal
  -- putHtmlLn "Final prediction models"
  -- describePredictorModels wainFinal
  putHtmlLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putHtmlLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putHtmlLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

describeClassifierModels :: ImageWain -> IO ()
describeClassifierModels w = mapM_ (putHtml . f) ms >> putHtmlLn ""
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, r) = show l ++ ": <img src='data:image/png;base64,"
                     ++ base64encode r ++ "'/>"

describePredictorModels :: ImageWain -> IO ()
describePredictorModels w = describePredictorModels' ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w

describePredictorModels' :: [(Label, Response Action)] -> IO ()
describePredictorModels' [] = return ()
describePredictorModels' xs = do
  putHtmlLn $ concatMap f ys
  describePredictorModels' zs
  where (ys, zs) = splitAt 4 xs
        f (l, r) = show l ++ ": " ++ pretty r ++ " "

dir :: String
dir = "/home/eamybut/mnist/testData/"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readImage2 :: FilePath -> IO (Object Action)
readImage2 f = do
  img <- readImage f
  return $ IObject img (takeFileName f)

main :: IO ()
main = do
  putHtmlLn "By about round 100, the wain should have a good set of"
  putHtmlLn "classifier models, which should change little after that."
  putHtmlLn "By about round 500, the predictor should have all the"
  putHtmlLn "correct actions."
  putHtmlLn "The predictor models will continue to vary, but the correct"
  putHtmlLn "actions should not change."
  putHtmlLn "After round 500, the only mistakes the wain should make are"
  putHtmlLn "for numerals that are 'borderline'."
  files <- take 1000 . drop 2 <$> readDirAndShuffle dir
  imgs <- mapM readImage2 files
  _ <- foldM tryOne testWain imgs
  putHtmlLn "test complete"

