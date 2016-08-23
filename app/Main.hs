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

data Answer = Answer { actualValue :: S.Scientific, whatTheMachineThought :: S.Scientific, difference :: S.Scientific }  deriving (Show, Generic)

instance Aeson.ToJSON Answer

floatToRational f = let (significand, exponent) = decodeFloat f
                    in ((fromIntegral significand) * (fromIntegral 2) ^^ exponent)

makeResponse floatingInput stringInput =
    let impreciseScientificInput = fromRational $ floatToRational floatingInput
        scientificInput :: S.Scientific
        scientificInput = read stringInput
        error = impreciseScientificInput - scientificInput
    in json $ Answer {actualValue = scientificInput, whatTheMachineThought = impreciseScientificInput, difference = error}

readme = "Your computer represents non-whole numbers imprecisely.\
\  It does this so that it can use a bounded amount of space, and so that\
\ numbers will be easy to add and multiply.  This is a tool to show you\
\ how a given number is represented within your computer (and how that differs from what you thought).  \
\  The \"float\" route corresponds to a 32-bit representation and the \"double\" route corresponds\
\ to a 64-bit representation"

sourceLink = "<a href=https://github.com/skatenerd/what-is-it-really/blob/master/app/Main.hs>source</a>"

main = scotty 80 $ do

  get "/" $ do
   html $ mconcat ["<h1><a href=\"/float/2.2345\">GET /float/2.2345</a></h1>", "<h1><a href=\"/double/2.2345\">GET /double/2.2345</a></h1>", "<h3>", readme, "</h3>", sourceLink]

  get "/float/:f" $ do
    floatInput :: Float <- param "f"
    stringInput :: String <- param "f"
    makeResponse floatInput stringInput

  get "/double/:f" $ do
    doubleInput :: Double <- param "f"
    stringInput :: String <- param "f"
    makeResponse doubleInput stringInput
