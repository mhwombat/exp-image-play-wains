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
    ImageThinker(..),
    run,
    randomImageWain,
    finishRound,
    schemaQuality,
    printStats,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive)
import ALife.Creatur.Counter (current, increment)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Wain.Brain (decider, Brain(..))
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.Decider (buildDecider)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, numModels, schemaQuality, toList, Label)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, responseSet, action,
  outcome, scenario)
import ALife.Creatur.Wain.PlusMinusOne (pm1ToDouble)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Util (unitInterval)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Interaction.Action (Action(..))
import qualified ALife.Creatur.Wain.Interaction.FMRI as F
import ALife.Creatur.Wain.Interaction.Image (Image, bigX,
  randomImageR)
import ALife.Creatur.Wain.Interaction.ImageThinker (ImageThinker(..))
import ALife.Creatur.Wain.Interaction.ImageDB (ImageDB, anyImage)
import qualified ALife.Creatur.Wain.Interaction.Universe as U
import ALife.Creatur.Wain.Interaction.Wain (Wain,
  buildWainAndGenerateGenome, appearance, name, chooseAction, incAge,
  applyMetabolismCost, weanMatureChildren, pruneDeadChildren,
  adjustEnergy, autoAdjustBoredom, adjustBoredom, autoAdjustPassion,
  reflect, mate, litter, brain, energy, childEnergy, age, wainSize,
  happiness)
import ALife.Creatur.Persistent (getPS, putPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (replicateM, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandomRs,
  getRandoms, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import Text.Printf (printf)

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

objectChildEnergy :: Object -> UIDouble
objectChildEnergy (IObject _ _) = 0
objectChildEnergy (AObject a) = childEnergy a

addIfAgent :: Object -> [ImageWain] -> [ImageWain]
addIfAgent (IObject _ _) xs = xs
addIfAgent (AObject a) xs = a:xs

type ImageWain = Wain Image ImageThinker  Action

randomImageWain
  :: RandomGen r
    => String -> U.Universe ImageWain -> Word16 -> Rand r ImageWain
randomImageWain wainName u classifierSize = do
  let w = view U.uImageWidth u
  let h = view U.uImageHeight u
  let r = view U.uInitialImageRange u
  imgs <- replicateM (fromIntegral classifierSize) (randomImageR w h r)
  let fcp = RandomExponentialParams
               { _r0Range = view U.uClassifierR0Range u,
                 _dRange = view U.uClassifierDRange u }
  fc <- randomExponential fcp
  let c = buildClassifier fc ImageThinker imgs
  let fdp = RandomExponentialParams
              { _r0Range = view U.uDeciderR0Range u,
                _dRange = view U.uDeciderDRange u }
  fd <- randomExponential fdp
  -- xs <- replicateM (fromIntegral deciderSize) $
  --        randomResponse 2 (numModels c) 4 (view U.uOutcomeRange u)
  xs <- responseSet 2 (numModels c) 4
  cw <- (makeWeights . take 3) <$> getRandoms
  sw <- (makeWeights . take 3) <$> getRandoms
  rw <- (makeWeights . take 2) <$> getRandoms
  let dr = buildDecider fd cw sw rw xs
  hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let b = Brain c dr hw
  dv <- getRandomR . view U.uDevotionRange $ u
  m <- getRandomR . view U.uMaturityRange $ u
  p <- getRandomR unitInterval
  bd <- getRandomR unitInterval
  let app = bigX w h
  return $ buildWainAndGenerateGenome wainName app b dv m p bd

data Summary = Summary
  {
    _rPopSize :: Int,
    _rOtherNovelty :: UIDouble,
    _rOtherAdjustedNovelty :: Int,
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
    _rOtherNovelty = 0,
    _rOtherAdjustedNovelty = 0,
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
    Stats.dStat "novelty" (view rOtherNovelty r),
    Stats.iStat "novelty (adj.)"
      (view rOtherAdjustedNovelty r),
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
  applyPopControl
  r <- chooseSubjectAction
  happinessBefore <- happiness <$> use subject
  runAction (view action r)
  letSubjectReflect happinessBefore r
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
  zoom universe . U.writeToLog $ "bms=" ++ show bms ++ " cps=" ++ show cps ++ " adult size=" ++ show (view wainSize a) ++ " adult cost=" ++ show adultCost
  (summary . rMetabolismDeltaE) += adultCost
  (summary . rChildMetabolismDeltaE) += childCost
  assign subject a'

chooseSubjectAction
  :: StateT Experiment IO (Response Action)
chooseSubjectAction = do
  a <- use subject
  obj <- use other
  (objNovelty, objNoveltyAdj, r, a')
    <- zoom universe $ chooseAction3 a obj
  assign (summary.rOtherNovelty) objNovelty
  assign (summary.rOtherAdjustedNovelty) objNoveltyAdj
  assign subject a'
  return r

chooseAction3
  :: ImageWain -> Object
    -> StateT (U.Universe ImageWain) IO
        (UIDouble, Int, Response Action, ImageWain)
chooseAction3 w obj = do
  U.writeToLog $ agentId w ++ " sees " ++ objectId obj
  whenM (use U.uShowDeciderModels) $ describeModels w
  let (objLabel:_, scenarioLabel, r, w', xs, [objNovelty])
        = chooseAction [objectAppearance obj] w
  whenM (use U.uGenFmris) (writeFmri w)
  U.writeToLog $ "scenario=" ++ pretty (view scenario r)
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId obj ++ " has novelty " ++ show objNovelty
    ++ " and best fits classifier model " ++ show objLabel
  whenM (use U.uShowPredictions) $ describeOutcomes w xs
  let objNoveltyAdj
        = round $ uiToDouble objNovelty * fromIntegral (view age w)
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ objectId obj ++ " has adjusted novelty " ++ show objNoveltyAdj
  U.writeToLog $ agentId w ++ " sees " ++ objectId obj
    ++ " and chooses to " ++ show (view action r)
    ++ " based on response model " ++ show scenarioLabel
  -- let modelsBefore = models $ view (brain . classifier) w
  -- let modelsAfter = models $ view (brain . classifier) w'
  -- U.writeToLog $ "DEBUG classifier model changes = "
  --   ++ show (modelChanges modelsBefore modelsAfter)
  return (objNovelty, objNoveltyAdj, r, w')

writeFmri :: ImageWain -> StateT (U.Universe ImageWain) IO ()
writeFmri w = do
  t <- U.currentTime
  k <- zoom U.uFmriCounter current
  zoom U.uFmriCounter increment
  d <- use U.uFmriDir
  let f = d ++ "/" ++ view name w ++ '_' : show t ++ "_" ++ show k ++ ".png"
  U.writeToLog $ "Writing FMRI to " ++ f
  liftIO . F.writeFmri w $ f

describeModels :: ImageWain -> StateT (U.Universe ImageWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . view decider $ view brain w
        f (l, r) = view name w ++ "'s decider model " ++ show l ++ "="
                     ++ pretty r

describeOutcomes
  :: ImageWain -> [(Response Action, Label)]
    -> StateT (U.Universe ImageWain) IO ()
describeOutcomes w = mapM_ (U.writeToLog . f)
  where f (r, l) = view name w ++ "'s predicted outcome of "
                     ++ show (view action r) ++ " is "
                     ++ (printf "%.3f" . pm1ToDouble . fromJust
                          . view outcome $ r)
                     ++ " from model " ++ show l

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
runAction Ignore = do
  (summary.rIgnoreCount) += 1

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

    let (Just adultNet) = Stats.lookup "avg. adult net Δe" xs
    U.writeToLog $ "adultNet=" ++ show adultNet
    let (Just childNet) = Stats.lookup "avg. child net Δe" xs
    U.writeToLog $ "childNet=" ++ show childNet

    let (Just adultPopControl)
          = Stats.lookup "avg. adult pop. control Δe" xs
    U.writeToLog $ "adultPopControl=" ++ show adultPopControl
    let (Just childPopControl)
          = Stats.lookup "avg. child pop. control Δe" xs
    U.writeToLog $ "childPopControl=" ++ show childPopControl

    let avgEnergyToBalance 
          = adultNet + childNet - adultPopControl - childPopControl
    U.writeToLog $ "avgEnergyToBalance=" ++ show avgEnergyToBalance
    let c = idealPopControlDeltaE idealPop pop avgEnergyToBalance
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

idealPopControlDeltaE :: Int -> Int -> Double -> Double
idealPopControlDeltaE idealPop pop e
  | idealPop == 0 = error "idealPop == 0"
  | pop == 0      = error "pop == 0"
  | otherwise    = -f*e
  where f = if e < 0
              then fromIntegral idealPop / fromIntegral pop
              else fromIntegral pop / fromIntegral idealPop

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view energy <$> use subject
  b <- fmap uiToDouble $ objectEnergy <$> use other
  d <- fmap uiToDouble $ childEnergy <$> use subject
  e <- fmap uiToDouble $ objectChildEnergy <$> use other
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
  :: UIDouble -> Response Action -> StateT Experiment IO ()
letSubjectReflect happinessBefore r = do
  x <- use subject
  p <- objectAppearance <$> use other
  happinessAfter <- happiness <$> use subject
  let (x', err) = reflect [p] r x
  assign subject x'
  assign (summary . rErr) err
  when (happinessAfter < happinessBefore) $ do
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
