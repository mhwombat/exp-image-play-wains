------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Interaction.Experiment
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Interaction.Experiment
  (
    ImageWain,
    ImageTweaker(..),
    run,
    randomImageWain,
    finishRound,
    schemaQuality,
    printStats,
    -- items below are only exported for testing
    idealPopControlDeltaE,
    Object(..),
    isImage,
    objectId,
    objectNum,
    objectAppearance
  ) where

import ALife.Creatur (agentId, isAlive)
-- import ALife.Creatur.Counter (current, increment)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Wain.Brain (classifier, predictor, makeBrain,
  scenarioReport, responseReport, decisionReport)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, schemaQuality, modelMap)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, action,
  outcome, scenario)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Interaction.Action (Action(..), numActions)
-- import qualified ALife.Creatur.Wain.Interaction.FMRI as F
import ALife.Creatur.Wain.Interaction.Image (Image, bigX, base64encode)
import ALife.Creatur.Wain.Interaction.ImageTweaker (ImageTweaker(..))
import ALife.Creatur.Wain.Interaction.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.Interaction.Universe as U
import ALife.Creatur.Wain (Wain, buildWainAndGenerateGenome, appearance,
  name, chooseAction, incAge, applyMetabolismCost, weanMatureChildren,
  pruneDeadChildren, adjustEnergy, autoAdjustBoredom, adjustBoredom,
  autoAdjustPassion, reflect, mate, litter, brain, energy, childEnergy,
  boredom, passion, age, wainSize, happiness)
import ALife.Creatur.Persistent (putPS, getPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandom,
  getRandoms, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.List (intercalate, minimumBy)
import Data.Map.Strict (toList)
import Data.Ord (comparing)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

data Object = IObject Image String | AObject ImageWain

isImage :: Object -> Bool
isImage (IObject _ _) = True
isImage (AObject _) = False

objectId :: Object -> String
objectId (IObject _ s) = "Image " ++ s
objectId (AObject a) = agentId a

objectNum :: Object -> Int
objectNum (IObject _ s) = read [head s]
objectNum (AObject _) = 10

objectAppearance :: Object -> Image
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = view appearance a

objectEnergy :: Object -> UIDouble
objectEnergy (IObject _ _) = 0
objectEnergy (AObject a) = view energy a

objectChildEnergy :: Object -> Double
objectChildEnergy (IObject _ _) = 0
objectChildEnergy (AObject a) = childEnergy a

addIfAgent :: Object -> [ImageWain] -> [ImageWain]
addIfAgent (IObject _ _) xs = xs
addIfAgent (AObject a) xs = a:xs

type ImageWain = Wain Image ImageTweaker Action

randomImageWain
  :: RandomGen r
    => String -> U.Universe ImageWain -> Word16 -> Rand r ImageWain
randomImageWain wainName u classifierSize = do
  let w = view U.uImageWidth u
  let h = view U.uImageHeight u
  let fcp = RandomExponentialParams
               { _r0Range = view U.uClassifierR0Range u,
                 _dRange = view U.uClassifierDRange u }
  fc <- randomExponential fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  let c = Cl.buildClassifier fc classifierSize classifierThreshold
            ImageTweaker
  let fdp = RandomExponentialParams
              { _r0Range = view U.uPredictorR0Range u,
                _dRange = view U.uPredictorDRange u }
  fd <- randomExponential fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  cw <- (makeWeights . take 3) <$> getRandoms
  rw <- (makeWeights . take 2) <$> getRandoms
  let predictorSize = classifierSize * fromIntegral numActions
  let dr = buildPredictor fd predictorSize predictorThreshold cw rw
  -- TODO: Allow a range of random weights
  -- hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let hw = makeWeights [0.7, 0.3, 0.1]
  dOut <- getRandomR $ view U.uDefaultOutcomeRange u
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  let b = makeBrain c mr dr hw
  dv <- getRandomR . view U.uDevotionRange $ u
  m <- getRandomR . view U.uMaturityRange $ u
  p <- getRandom
  bd <- getRandom
  let app = bigX w h
  return $ buildWainAndGenerateGenome wainName app b dv m p bd

data Summary = Summary
  {
    _rPopSize :: Int,
    _rMetabolismDeltaE :: Double,
    _rChildMetabolismDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rChildPopControlDeltaE :: Double,
    _rEatDeltaE :: Double,
    _rChildEatDeltaE :: Double,
    _rPlayDeltaB :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rChildOldAgeDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
    _rDeltaEToReflectOn :: Double,
    _rDeltaBToReflectOn :: Double,
    _rDeltaPToReflectOn :: Double,
    _rDeltaHToReflectOn :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rEatCount :: Int,
    _rPlayCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rIgnoreCount :: Int,
    _rDeathCount :: Int,
    _rMistakeCount :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rMetabolismDeltaE = 0,
    _rChildMetabolismDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rChildPopControlDeltaE = 0,
    _rEatDeltaE = 0,
    _rChildEatDeltaE = 0,
    _rPlayDeltaB = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rChildOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rDeltaEToReflectOn = 0,
    _rDeltaBToReflectOn = 0,
    _rDeltaPToReflectOn = 0,
    _rDeltaHToReflectOn = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rEatCount = 0,
    _rPlayCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rIgnoreCount = 0,
    _rDeathCount = 0,
    _rMistakeCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "child metabolism Δe" (view rChildMetabolismDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "child pop. control Δe" (view rChildPopControlDeltaE r),
    Stats.dStat "adult eat Δe" (view rEatDeltaE r),
    Stats.dStat "child eat Δe" (view rChildEatDeltaE r),
    Stats.dStat "adult play Δb" (view rPlayDeltaB r),
    Stats.dStat "adult flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "child old age Δe" (view rChildOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "Δe to reflect on" (view rDeltaEToReflectOn r),
    Stats.dStat "Δb to reflect on" (view rDeltaBToReflectOn r),
    Stats.dStat "Δp to reflect on" (view rDeltaPToReflectOn r),
    Stats.dStat "Δh to reflect on" (view rDeltaHToReflectOn r),
    Stats.dStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "ate" (view rEatCount r),
    Stats.iStat "played" (view rPlayCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "ignored" (view rIgnoreCount r),
    Stats.iStat "died" (view rDeathCount r),
    Stats.iStat "mistakes" (view rMistakeCount r)
  ]

data Experiment = Experiment
  {
    _subject :: ImageWain,
    _other :: Object,
    _weanlings :: [ImageWain],
    _universe :: U.Universe ImageWain,
    _summary :: Summary
  }
makeLenses ''Experiment

run :: [ImageWain] -> StateT (U.Universe ImageWain) IO [ImageWain]
run (me:w2:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last wain standing!"
  u <- get
  x <- liftIO $ chooseObject (view U.uFrequencies u) w2
                       (view U.uImageDB u)
  p <- U.popSize
  let e = Experiment { _subject = me,
                       _other = x,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  let modifiedAgents = addIfAgent (view other e')
            $ view subject e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  reportAnyDeaths modifiedAgents
  return modifiedAgents
run _ = error "too few wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  zoom universe . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  zoom universe . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  runMetabolism
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  r <- chooseSubjectAction
  wainBeforeAction <- use subject
  runAction (view action r)
  letSubjectReflect wainBeforeAction r
  autoAdjustSubjectPassion
  autoAdjustSubjectBoredom
  subject %= incAge
  a' <- use subject
  zoom universe . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  zoom universe . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rMetabolismDeltaE s
         + _rPopControlDeltaE s
         + _rEatDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s,
    _rChildNetDeltaE = _rChildMetabolismDeltaE s
         + _rChildPopControlDeltaE s
         + _rChildEatDeltaE s
         + _rChildOldAgeDeltaE s
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Adult energy equation doesn't balance"
    zoom universe . U.writeToLog $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Child energy equation doesn't balance"
    zoom universe . U.writeToLog $ "ec0=" ++ show ec0
      ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bms <- use (universe . U.uBaseMetabolismDeltaE)
  cps <- use (universe . U.uEnergyCostPerByte)
  ccf <- use (universe . U.uChildCostFactor)
  let (a', adultCost, childCost) = applyMetabolismCost bms cps ccf a
  zoom universe . U.writeToLog $ "bms=" ++ show bms
    ++ " cps=" ++ show cps ++ " adult size=" ++ show (view wainSize a)
    ++ " adult cost=" ++ show adultCost
    ++ " adult energy after=" ++ show (view energy a')
    ++ " alive=" ++ show (isAlive a')
  (summary . rMetabolismDeltaE) += adultCost
  (summary . rChildMetabolismDeltaE) += childCost
  assign subject a'

chooseSubjectAction
  :: StateT Experiment IO (Response Action)
chooseSubjectAction = do
  a <- use subject
  obj <- use other
  (r, a') <- zoom universe $ chooseAction3 a obj
  assign subject a'
  return r

chooseAction3
  :: ImageWain -> Object
    -> StateT (U.Universe ImageWain) IO
        (Response Action, ImageWain)
chooseAction3 w obj = do
  U.writeToLog $ agentId w ++ " sees " ++ objectId obj
  whenM (use U.uShowPredictorModels) $ describePredictorModels w
  let (lds, sps, rplos, aos, r, w')
        = chooseAction [objectAppearance obj] w
  let objLabel = analyseClassification lds
  -- whenM (use U.uGenFmris) (writeFmri w)
  whenM (use U.uGenFmris) (describeClassifierModels w)
  U.writeToLog $ "scenario=" ++ pretty (view scenario r)
  whenM (use U.uShowPredictions) $ do
    mapM_ U.writeToLog $ scenarioReport sps
    mapM_ U.writeToLog $ responseReport rplos
    mapM_ U.writeToLog $ decisionReport aos
  U.writeToLog $  agentId w ++ " sees " ++ objectId obj
    ++ ", classifies it as " ++ show objLabel ++ " and chooses to "
    ++ show (view action r) ++ " predicting the outcome "
    ++ show (view outcome r)
  return (r, w')

analyseClassification
  :: [[(Cl.Label, Cl.Difference)]] -> Cl.Label
analyseClassification ldss = l
  where ((l, _):_) = map (minimumBy (comparing snd)) ldss

describeClassifierModels :: ImageWain -> StateT (U.Universe ImageWain) IO ()
describeClassifierModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . modelMap . view (brain . classifier) $ w
        f (l, r) = view name w ++ "'s classifier model "
                     ++ show l ++ ": <img src='data:image/png;base64,"
                     ++ base64encode r ++ "'/>"

describePredictorModels :: ImageWain -> StateT (U.Universe ImageWain) IO ()
describePredictorModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . modelMap . view (brain . predictor) $ w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ ": "
                     ++ pretty r

chooseObject :: [Rational] -> ImageWain -> ImageDB -> IO Object
chooseObject freqs w db = do
  (img1, imageId1) <- evalStateT anyImage db
  fromList $ zip [IObject img1 imageId1, AObject w] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Eat
--
runAction Eat = do
  obj <- use other
  let n = objectNum obj
  deltaEs <- use (universe . U.uInteractionDeltaE)
  adjustSubjectEnergy (deltaEs !! n) rEatDeltaE rChildEatDeltaE
  (summary.rEatCount) += 1

--
-- Play
--
runAction Play = do
  obj <- use other
  let n = objectNum obj
  deltaBs <- use (universe . U.uInteractionDeltaB)
  adjustSubjectBoredom (deltaBs !! n) rPlayDeltaB
  (summary.rPlayCount) += 1

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  obj <- use other
  unless (isImage obj) flirt
  (summary.rFlirtCount) += 1

--
-- Ignore
--
runAction Ignore = (summary.rIgnoreCount) += 1

--
-- Utility functions
--

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  adjustSubjectEnergy deltaE rPopControlDeltaE
    rChildPopControlDeltaE

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use other
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ mate a b babyName
  if null msgs
    then do
      zoom universe . U.writeToLog $
        agentId a ++ " and " ++ agentId b ++ " mated"
      assign subject a'
      assign other (AObject b')
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (zoom universe . U.writeToLog) msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- use (universe . U.uFlirtingDeltaE)
  adjustSubjectEnergy deltaE rFlirtingDeltaE undefined

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    adjustSubjectEnergy (-100) rOldAgeDeltaE rChildOldAgeDeltaE

finishRound :: FilePath -> StateT (U.Universe ImageWain) IO ()
finishRound f = do
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uPopulationAllowedRange
  checkPopSize (a, b)

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe ImageWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    pop <- U.popSize
    U.writeToLog $ "pop=" ++ show pop
    idealPop <- use U.uIdealPopulationSize
    U.writeToLog $ "ideal pop=" ++ show idealPop
    energyToAddWain <- use U.uEnergyToAddWain
    U.writeToLog $ "energy to add one wain=" ++ show energyToAddWain
    let c = idealPopControlDeltaE idealPop pop energyToAddWain
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

idealPopControlDeltaE :: Int -> Int -> Double -> Double
idealPopControlDeltaE idealPop pop energyToAddWain
  = energyToAddWain*fromIntegral (idealPop - pop) / fromIntegral pop

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view energy <$> use subject
  b <- fmap uiToDouble $ objectEnergy <$> use other
  d <- childEnergy <$> use subject
  e <- objectChildEnergy <$> use other
  return (a + b, d + e)

printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double
    -> Simple Lens Summary Double -> StateT Experiment IO ()
adjustSubjectEnergy deltaE adultSelector childSelector = do
  x <- use subject
  let (x', adultDeltaE, childDeltaE)
        = adjustEnergy deltaE x
  (summary . adultSelector) += adultDeltaE
  when (childDeltaE /= 0) $
    (summary . childSelector) += childDeltaE
  assign subject x'

adjustSubjectBoredom
  :: Double -> Simple Lens Summary Double -> StateT Experiment IO ()
adjustSubjectBoredom deltaB adultSelector = do
  x <- use subject
  let (x', deltaB') = adjustBoredom deltaB x
  (summary . adultSelector) += deltaB'
  assign subject x'

autoAdjustSubjectBoredom
  :: StateT Experiment IO ()
autoAdjustSubjectBoredom = subject %= autoAdjustBoredom

autoAdjustSubjectPassion
  :: StateT Experiment IO ()
autoAdjustSubjectPassion = subject %= autoAdjustPassion

letSubjectReflect
  :: ImageWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wainBefore r = do
  x <- use subject
  p <- objectAppearance <$> use other
  let energyBefore = view energy wainBefore
  let boredomBefore = view boredom wainBefore
  let passionBefore = view passion wainBefore
  let happinessBefore = happiness wainBefore
  energyAfter <- use (subject.energy)
  boredomAfter <- use (subject.boredom)
  passionAfter <- use (subject.passion)
  happinessAfter <- happiness <$> use subject
  let deltaH = uiToDouble happinessAfter - uiToDouble happinessBefore
  assign (summary . rDeltaEToReflectOn)
    (uiToDouble energyAfter - uiToDouble energyBefore)
  assign (summary . rDeltaBToReflectOn)
    (uiToDouble boredomAfter - uiToDouble boredomBefore)
  assign (summary . rDeltaPToReflectOn)
    (uiToDouble passionAfter - uiToDouble passionBefore)
  assign (summary . rDeltaHToReflectOn) deltaH
  let (x', err) = reflect [p] r x
  assign subject x'
  assign (summary . rErr) err
  when (deltaH < 0) $ do
    b <- use other
    zoom universe . U.writeToLog $
      agentId x ++ "'s choice to " ++ show (view action r) ++ " (with) "
        ++ objectId b ++ " was a mistake"
    (summary . rMistakeCount) += 1

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe ImageWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths :: [ImageWain] -> StateT (U.Universe ImageWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age " ++ show (view age w))
