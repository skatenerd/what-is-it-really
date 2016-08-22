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


-- The denominator of (floatTorational x) is always going to be a power of 2.
-- This is beause the only opportunity for us to have a denominator at all
-- comes from having a negative "exponent" component.
-- If we have a negative "exponent", then our number is just of the form
-- N / 2 ^ k
floatToRational f = let (significand, exponent) = decodeFloat f
                    in ((fromIntegral significand) * (fromIntegral 2) ^^ exponent)


insertBefore  items element index = concat [(take index items), [element], (drop index items)]
insertFromEnd items element countFromEnd = insertBefore  items element $ (length items) - countFromEnd


-- Notice that, whenever a floating point is represented as a rational (see note on floatToRational)
-- This lets us do a clever hack and put the rational into base 10
-- simply by multiplying top and bottom through by a bunch of fives.
-- Once we have it in base 10, it's easy to figure out how the number looks as a string.
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
        -- we are never going to hit the "left" case in the reader (R.rational textInput),
        -- since the endpoint specification has already rejectd non-numeric inputs
        actualRepresentation :: Rational
        actualRepresentation = (floatToRational floatingInput)
        rationalDifference = rationalInput - actualRepresentation
    in json $ Answer {actualValue = (showFloat floatingInput), original = stringInput, approxDifference = (fromRational rationalDifference)}

main = scotty 3000 $ do

  get "/" $ do
   html $ mconcat ["<h1>GET /float/2.2345</h1>", "<h1>GET /double/2.2345</h1>"]

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
