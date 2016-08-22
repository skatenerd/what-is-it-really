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
import qualified Data.Scientific as S

data Answer = Answer { actualValue :: S.Scientific, original :: S.Scientific, difference :: S.Scientific }  deriving (Show, Generic)

instance Aeson.ToJSON Answer

floatToRational f = let (significand, exponent) = decodeFloat f
                    in ((fromIntegral significand) * (fromIntegral 2) ^^ exponent)

makeResponse floatingInput stringInput =
    let impreciseScientificInput = fromRational $ floatToRational floatingInput
        scientificInput :: S.Scientific
        scientificInput = read stringInput
        error = impreciseScientificInput - scientificInput
    in json $ Answer {actualValue = scientificInput, original = impreciseScientificInput, difference = error}

main = scotty 3000 $ do

  get "/" $ do
   html $ mconcat ["<h1><a href=\"/float/2.2345\">GET /float/2.2345</a></h1>", "<h1><a href=\"/double/2.2345\">GET /double/2.2345</a></h1>"]

  get "/float/:f" $ do
    floatInput :: Float <- param "f"
    stringInput :: String <- param "f"
    makeResponse floatInput stringInput

  get "/double/:f" $ do
    doubleInput :: Double <- param "f"
    stringInput :: String <- param "f"
    makeResponse doubleInput stringInput
