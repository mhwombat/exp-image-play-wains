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
    idealPopControlDeltaE
  ) where

import ALife.Creatur (agentId, isAlive)
-- import ALife.Creatur.Counter (current, increment)
import ALife.Creatur.Task (checkPopSize)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Brain (makeBrain, scenarioReport,
  responseReport, decisionReport)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, schemaQuality)
import qualified ALife.Creatur.Wain.Object as O
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, action, outcomes)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Interaction.Action (Action(..), numActions)
-- import qualified ALife.Creatur.Wain.Interaction.FMRI as F
import ALife.Creatur.Wain.Image (bigX)
import ALife.Creatur.Wain.ImageTweaker (ImageTweaker(..))
import ALife.Creatur.Wain.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.ImageWain as IW
import qualified ALife.Creatur.Wain.Interaction.Universe as U
import ALife.Creatur.Persistent (putPS, getPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandomRs,
  evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

type ImageWain = IW.ImageWain Action
type Object = O.Object Action

randomImageWain
  :: RandomGen r
    => String -> U.Universe ImageWain -> Word16 -> Rand r ImageWain
randomImageWain wName u classifierSize = do
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
  let predictorSize = classifierSize * fromIntegral numActions
  let dr = buildPredictor fd predictorSize predictorThreshold
  -- TODO: Allow a range of random weights
  -- hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let hw = makeWeights [0.7, 0.3, 0.1]
  dOut <- take 3 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  let wBrain = makeBrain c mr dr hw
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  wPassionDelta <- getRandomR . view U.uBoredomDeltaRange $ u
  wBoredomDelta <- getRandomR . view U.uPassionDeltaRange $ u
  let wAppearance = bigX w h
  return $ W.buildWainAndGenerateGenome wName wAppearance wBrain
    wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta

data Summary = Summary
  {
    _rPopSize :: Int,
    _rMetabolismDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rEatDeltaE :: Double,
    _rPlayDeltaB :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
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
    _rPopControlDeltaE = 0,
    _rEatDeltaE = 0,
    _rPlayDeltaB = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
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
    Stats.dStat "metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "eat Δe" (view rEatDeltaE r),
    Stats.dStat "play Δb" (view rPlayDeltaB r),
    Stats.dStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "mating Δe" (view rMatingDeltaE r),
    Stats.dStat "old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "net Δe" (view rNetDeltaE r),
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

-- TODO: Is there a more lens-y way to do this? Maybe can combine a
-- prism and a lens.
otherWain :: Lens' Experiment ImageWain
otherWain = lens getWain setWain
  where
    getWain :: Experiment -> ImageWain
    getWain = O.objectToWain . _other

    setWain :: Experiment -> ImageWain -> Experiment
    setWain e w = e { _other = O.AObject w }

report :: String -> StateT Experiment IO ()
report = zoom universe . U.writeToLog

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
  let modifiedAgents = O.addIfWain (view other e')
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
  report $ "---------- " ++ agentId a ++ "'s turn ----------"
  report $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  runMetabolism
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  r <- chooseSubjectAction
  wainBeforeAction <- use subject
  runAction (view action r)
  letSubjectReflect wainBeforeAction r
  subject %= W.autoAdjustPassion
  subject %= W.autoAdjustBoredom
  subject %= W.incAge
  a' <- use subject
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  report $ "At end of turn, " ++ agentId a
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
    _rChildNetDeltaE = 0
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
    report $ "WARNING: Adult energy equation doesn't balance"
    report $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    report $ "WARNING: Child energy equation doesn't balance"
    report $ "ec0=" ++ show ec0 ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bmc <- use (universe . U.uBaseMetabolismDeltaE)
  cpcm <- use (universe . U.uEnergyCostPerClassifierModel)
  ccf <- use (universe . U.uChildCostFactor)
  let deltaE = IW.metabCost bmc cpcm 1 a
                 + sum (map (IW.metabCost bmc cpcm ccf)
                         (view W.litter a))
  IW.adjustEnergy subject deltaE rMetabolismDeltaE "metab." summary
    report

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
  whenM (use U.uShowPredictorModels)
    (mapM_ U.writeToLog . IW.describePredictorModels $ w)
  let (lds, sps, rplos, aos, r, w')
        = W.chooseAction [O.objectAppearance obj] w
  let objLabel = analyseClassification lds
  -- whenM (use U.uGenFmris) (writeFmri w)
  whenM (use U.uGenFmris)
    (mapM_ U.writeToLog . IW.describeClassifierModels $ w)
  whenM (use U.uShowPredictions) $ do
    mapM_ U.writeToLog $ scenarioReport sps
    mapM_ U.writeToLog $ responseReport rplos
    mapM_ U.writeToLog $ decisionReport aos
  U.writeToLog $  agentId w ++ " sees " ++ O.objectId obj
    ++ ", classifies it as " ++ show objLabel ++ " and chooses to "
    ++ show (view action r) ++ " predicting the outcomes "
    ++ show (view outcomes r)
  return (r, w')

analyseClassification
  :: [[(Cl.Label, Cl.Difference)]] -> Cl.Label
analyseClassification ldss = l
  where ((l, _):_) = map (minimumBy (comparing snd)) ldss

chooseObject :: [Rational] -> ImageWain -> ImageDB -> IO Object
chooseObject freqs w db = do
  (img1, imageId1) <- evalStateT anyImage db
  fromList $ zip [O.IObject img1 imageId1, O.AObject w] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Eat
--
runAction Eat = do
  obj <- use other
  let n = O.objectNum obj
  deltaEs <- use (universe . U.uInteractionDeltaE)
  IW.adjustEnergy subject (deltaEs !! n) rEatDeltaE "eating" summary
    report
  (summary.rEatCount) += 1

--
-- Play
--
runAction Play = do
  obj <- use other
  let n = O.objectNum obj
  deltaBs <- use (universe . U.uInteractionDeltaB)
  adjustSubjectBoredom (deltaBs !! n) rPlayDeltaB
  (summary.rPlayCount) += 1

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  obj <- use other
  unless (O.isImage obj) flirt
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
  IW.adjustEnergy subject deltaE rPopControlDeltaE
    "pop. control" summary report

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  b <- use otherWain
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ W.mate a b babyName
  if null msgs
    then do
      report $ agentId a ++ " and " ++ agentId b ++ " mated"
      report $ "Contribution to child: " ++
        agentId a ++ "'s share is " ++ show aMatingDeltaE ++ " " ++ 
        agentId b ++ "'s share is " ++ show bMatingDeltaE
      assign subject a'
      assign otherWain b'
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (report) msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view W.litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- use (universe . U.uFlirtingDeltaE)
  report $ "Applying flirtation energy adjustment"
  IW.adjustEnergy subject deltaE rFlirtingDeltaE "flirting" summary report
  (summary.rFlirtCount) += 1

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- W.weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- W.pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view W.age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    IW.adjustEnergy subject (-100) rOldAgeDeltaE "old age"
      summary report

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
  (a, b) <- use U.uAllowedPopulationRange
  checkPopSize (a, b)

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe ImageWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    pop <- U.popSize
    U.writeToLog $ "pop=" ++ show pop
    popRange <- use U.uIdealPopulationRange
    U.writeToLog $ "ideal pop range=" ++ show popRange
    let (Just avgEnergy) = Stats.lookup "avg. energy" xs
    let c = idealPopControlDeltaE avgEnergy popRange pop
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

idealPopControlDeltaE :: Double -> (Int, Int) -> Int -> Double
idealPopControlDeltaE avgEnergy (a, b) pop
  | pop < a   = 0.8 - avgEnergy
  | pop > b   = avgEnergy - 0.2
  | otherwise = 0.5 - avgEnergy

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ otherWainEnergy
  d <- W.childEnergy <$> use subject
  e <- otherChildEnergy
  return (a + b, d + e)

otherWainEnergy :: StateT Experiment IO UIDouble
otherWainEnergy = do
  x <- use other
  case x of
    O.AObject w -> return $ view W.energy w
    _           -> return 0
  
otherChildEnergy :: StateT Experiment IO Double
otherChildEnergy = do
  x <- use other
  case x of
    O.AObject w -> return $ W.childEnergy w
    _           -> return 0
  
printStats :: [[Stats.Statistic]] -> StateT (U.Universe ImageWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustSubjectBoredom
  :: Double -> Simple Lens Summary Double -> StateT Experiment IO ()
adjustSubjectBoredom deltaB adultSelector = do
  x <- use subject
  let (x', deltaB') = W.adjustBoredom deltaB x
  (summary . adultSelector) += deltaB'
  assign subject x'

letSubjectReflect
  :: ImageWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wainBefore r = do
  w <- use subject
  p <- O.objectAppearance <$> use other
  let energyBefore = view W.energy wainBefore
  let boredomBefore = view W.boredom wainBefore
  let passionBefore = view W.passion wainBefore
  let happinessBefore = W.happiness wainBefore
  energyAfter <- use (subject . W.energy)
  boredomAfter <- use (subject . W.boredom)
  passionAfter <- use (subject . W.passion)
  happinessAfter <- W.happiness <$> use subject
  let deltaH = uiToDouble happinessAfter - uiToDouble happinessBefore
  assign (summary . rDeltaEToReflectOn)
    (uiToDouble energyAfter - uiToDouble energyBefore)
  assign (summary . rDeltaBToReflectOn)
    (uiToDouble boredomAfter - uiToDouble boredomBefore)
  assign (summary . rDeltaPToReflectOn)
    (uiToDouble passionAfter - uiToDouble passionBefore)
  assign (summary . rDeltaHToReflectOn) deltaH
  let (w', err) = W.reflect [p] r wainBefore w
  assign subject w'
  assign (summary . rErr) err
  when (deltaH < 0) $ do
    b <- use other
    report $ agentId w ++ "'s choice to " ++ show (view action r)
      ++ " (with) " ++ O.objectId b ++ " was a mistake"
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
                  (agentId w ++ " dead at age " ++ show (view W.age w))
