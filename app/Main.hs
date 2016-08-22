{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Lib
import Web.Scotty
import Data.Monoid (mconcat)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Read as R
import qualified Data.Text as T
import Data.Ratio
import GHC.Generics

data Answer = Answer { actualValue :: String, original :: String, approxDifference :: Double }  deriving (Show, Generic)

instance Aeson.ToJSON Answer

floatToRational f = let (significand, exponent) = decodeFloat f
                    in ((fromIntegral significand) * (fromIntegral 2) ^^ exponent)


insertBefore  items element index = concat [(take index items), [element], (drop index items)]
insertFromEnd items element countFromEnd = insertBefore  items element $ (length items) - countFromEnd

showFloat f = let asRational = floatToRational f
                  rationalDenominator = denominator $ asRational
                  twosInBottom = floor $ logBase 2 $ fromIntegral rationalDenominator
                  rationalNumerator = numerator asRational
                  wholeNumerator :: Integer = rationalNumerator * (5 ^ twosInBottom)
              in if twosInBottom > 0
                    then insertFromEnd (show wholeNumerator) '.' twosInBottom
                    else (show wholeNumerator)

makeResponse stringInput textInput floatingInput =
    let rationalInput :: Rational
        rationalInput = either (const 0.0) fst $ R.rational textInput
        actualRepresentation :: Rational
        actualRepresentation = (floatToRational floatingInput)
        rationalDifference = rationalInput - actualRepresentation
    in json $ Answer {actualValue = (showFloat floatingInput), original = stringInput, approxDifference = (fromRational rationalDifference)}

main = scotty 3000 $ do
  get "/float/:f" $ do
    stringInput :: String <- param "f"
    textInput :: T.Text <- param "f"
    floatInput :: Float <- param "f"
    makeResponse stringInput textInput floatInput

  get "/double/:f" $ do
    stringInput :: String <- param "f"
    textInput :: T.Text <- param "f"
    doubleInput :: Double <- param "f"
    makeResponse stringInput textInput doubleInput
