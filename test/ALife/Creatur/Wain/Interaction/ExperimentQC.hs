------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Interaction.ExperimentQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.Interaction.ExperimentQC
  (
    test
  ) where

import ALife.Creatur.Wain.Interaction.Experiment
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose (0,1)

prop_idealPopControlDeltaE_counteracts_overpopulation
  :: Positive Int -> Positive Int -> Positive Int -> UIDouble -> Property
prop_idealPopControlDeltaE_counteracts_overpopulation
  (Positive pIdeal) (Positive deltaP) (Positive deltaP2) e
    = e > 0.8 ==> ec1 < 0 && ec2 < 0 && ec2 < ec1
  where p1 = pIdeal + deltaP
        p2 = p1 + deltaP2
        ec1 = idealPopControlDeltaE pIdeal p1 e
        ec2 = idealPopControlDeltaE pIdeal p2 e

prop_idealPopControlDeltaE_counteracts_underpopulation
  :: Positive Int -> Positive Int -> Positive Int -> UIDouble -> Property
prop_idealPopControlDeltaE_counteracts_underpopulation
  (Positive p1) (Positive deltaP) (Positive deltaP2) e
    = e < 0.8 ==> ec1 > 0 && ec2 > 0 && ec2 < ec1
  where p2 = p1 + deltaP
        pIdeal = p2 + deltaP2
        ec1 = idealPopControlDeltaE pIdeal p1 e
        ec2 = idealPopControlDeltaE pIdeal p2 e

prop_overpopulated_environment_gets_harsher_as_wains_learn
  :: Positive Int -> Positive Int -> UIDouble -> UIDouble -> Property
prop_overpopulated_environment_gets_harsher_as_wains_learn
  (Positive pIdeal) (Positive deltaP) e1 e2
    = 0.8 < e1 && e1 < e2 ==> ec1 < 0 && ec2 < 0 && ec2 < ec1
  where p = pIdeal + deltaP
        ec1 = idealPopControlDeltaE pIdeal p e1
        ec2 = idealPopControlDeltaE pIdeal p e2

prop_underpopulated_environment_gets_harsher_as_wains_learn
  :: Positive Int -> Positive Int -> UIDouble -> UIDouble -> Property
prop_underpopulated_environment_gets_harsher_as_wains_learn
  (Positive p) (Positive deltaP) e1 e2
    = e1 < e2 && e2 < 0.8 ==> ec1 > 0 && ec2 > 0 && ec2 < ec1
  where pIdeal = p + deltaP
        ec1 = idealPopControlDeltaE pIdeal p e1
        ec2 = idealPopControlDeltaE pIdeal p e2

test :: Test
test = testGroup "ALife.Creatur.Wain.Interaction.ExperimentQC"
  [
    testProperty "prop_idealPopControlDeltaE_counteracts_overpopulation"
      prop_idealPopControlDeltaE_counteracts_overpopulation,
    testProperty "prop_idealPopControlDeltaE_counteracts_underpopulation"
      prop_idealPopControlDeltaE_counteracts_underpopulation,
    testProperty "prop_overpopulated_environment_gets_harsher_as_wains_learn"
      prop_overpopulated_environment_gets_harsher_as_wains_learn,
    testProperty "prop_underpopulated_environment_gets_harsher_as_wains_learn"
      prop_underpopulated_environment_gets_harsher_as_wains_learn
  ]
