{-# LANGUAGE TemplateHaskell #-}
module UnitTests.Distribution.PackageDescription.PrettyPrint
    ( tests
    ) where

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint

hoist :: ParseResult a -> Maybe a
hoist (ParseOk _ x) = Just x
hoist (ParseFailed _) = Nothing

prop_readwriteread :: String -> Property
prop_readwriteread s =
    let desc = hoist . parsePackageDescription $ s
        desc' = desc >>= hoist . parsePackageDescription . showGenericPackageDescription
    in property $ isJust desc && desc == desc'

tests :: [TestTree]
tests =
    [ testProperty "accelerate-cude 0.14.0.0" $
          prop_readwriteread $ unpack $(embedFile "tests/UnitTests/Distribution/PackageDescription/accelerate-cuda.0.14.0.0.cabal")
    ]
