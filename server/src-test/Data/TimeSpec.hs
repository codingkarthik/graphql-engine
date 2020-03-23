module Data.TimeSpec (spec) where
-- | Time-related properties we care about.

import           Data.Aeson
import           Data.Time
import           Data.Time.Clock.Units
import           Prelude
import           Test.Hspec

spec :: Spec
spec = do
  --timeAbsoluteUnitsSpec
  timeCalendarUnitsSpec
  diffTimeSpec

class TimeUnitsSpec t where
  timeUnitSpec :: t -> Spec
  timeUnitSpec t =
    describe "time absolute units" $ do
      it "converts correctly" $ do
        (seconds 123 ::  t) `shouldBe` 123
        (milliseconds 123 ::  t) `shouldBe` 0.123
        (microseconds 123 ::  t) `shouldBe` 0.000123
        (nanoseconds 123 ::  t) `shouldBe` 0.000000123

      it "has a correct Read instance" $ do
        (seconds (read "123") ::  t) `shouldBe` 123
        (milliseconds (read "123") ::  t) `shouldBe` 0.123
        (microseconds (read "123") ::  t) `shouldBe` 0.000123
        (nanoseconds (read "123") ::  t) `shouldBe` 0.000000123

deriving instance TimeUnitsSpec (Duration 'Absolute)
deriving instance TimeUnitsSpec (Duration 'Calendar)

      -- it "JSON serializes as proper units" $ do
      --   toJSON (1 :: Seconds t) `shouldBe` Number 1
      --   decode "1.0" `shouldBe` Just (1 :: Seconds t)

timeCalendarUnitsSpec :: Spec
timeCalendarUnitsSpec =
  describe "time calendar units" $ do
    it "converts correctly" $ do
      (seconds 123 :: Duration 'Calendar) `shouldBe` 123
      (milliseconds 123 :: Duration 'Calendar) `shouldBe` 0.123
      (microseconds 123 :: Duration 'Calendar) `shouldBe` 0.000123
      (nanoseconds 123 :: Duration 'Calendar) `shouldBe` 0.000000123

    it "has a correct Read instance" $ do
      (seconds (read "123") :: Duration 'Calendar) `shouldBe` 123
      (milliseconds (read "123") :: Duration 'Calendar) `shouldBe` 0.123
      (microseconds (read "123") :: Duration 'Calendar) `shouldBe` 0.000123
      (nanoseconds (read "123") :: Duration 'Calendar) `shouldBe` 0.000000123

    it "JSON serializes as proper units" $ do
      toJSON (1 :: Seconds 'Calendar) `shouldBe` Number 1
      decode "1.0" `shouldBe` Just (1 :: Seconds 'Calendar)

    it "converts with fromUnits" $ do
      fromUnits (2 :: Minutes 'Calendar) `shouldBe` (120 :: DiffTime)
      fromUnits (60 :: Seconds 'Calendar) `shouldBe` (1 :: Minutes 'Absolute)

diffTimeSpec :: Spec
diffTimeSpec =
  describe "DiffTime" $ do
    it "JSON serializes as seconds" $ do
    -- although we would prefer to use Seconds instead...
      toJSON (1 :: DiffTime) `shouldBe` Number 1
      decode "1.0" `shouldBe` Just (1 :: DiffTime)
