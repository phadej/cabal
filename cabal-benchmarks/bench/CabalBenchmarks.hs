{-# OPTIONS -Wno-deprecations #-}
module Main where

import Criterion.Main                         (bench, bgroup, defaultMain, env, nf, whnf)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec                    (eitherParsec)
import Distribution.Version                   (VersionRange, normaliseVersionRange)

import qualified Data.ByteString as BS

import qualified Distribution.Types.VersionInterval.Legacy as Old
import qualified Distribution.Types.VersionInterval        as New

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ env (BS.readFile "Cabal/Cabal.cabal") $ \bs ->
      bench "Cabal" $ whnf parseGenericPackageDescriptionMaybe bs
    , env (BS.readFile "cabal-benchmarks/cabal-benchmarks.cabal") $ \bs ->
      bench "cabal-benchmarks" $ whnf parseGenericPackageDescriptionMaybe bs

    , bgroup "normaliseVersionRange"
        [ bgroup "def"
            [ env bigVersionRange1 $ \vr -> bench "dnf1" $ nf normaliseVersionRange vr
            , env bigVersionRange2 $ \vr -> bench "cnf1" $ nf normaliseVersionRange vr
            , env bigVersionRange3 $ \vr -> bench "dnf2" $ nf normaliseVersionRange vr
            , env bigVersionRange4 $ \vr -> bench "cnf2" $ nf normaliseVersionRange vr
            ]
        , bgroup "old"
            [ env bigVersionRange1 $ \vr -> bench "dnf1" $ nf oldNormaliseVersionRange vr
            , env bigVersionRange2 $ \vr -> bench "cnf1" $ nf oldNormaliseVersionRange vr
            , env bigVersionRange3 $ \vr -> bench "dnf2" $ nf oldNormaliseVersionRange vr
            , env bigVersionRange4 $ \vr -> bench "cnf2" $ nf oldNormaliseVersionRange vr
            ]
        , bgroup "new"
            [ env bigVersionRange1 $ \vr -> bench "dnf1" $ nf newNormaliseVersionRange vr
            , env bigVersionRange2 $ \vr -> bench "cnf1" $ nf newNormaliseVersionRange vr
            , env bigVersionRange3 $ \vr -> bench "dnf2" $ nf newNormaliseVersionRange vr
            , env bigVersionRange4 $ \vr -> bench "cnf2" $ nf newNormaliseVersionRange vr
            ]
        ]
    ]

-------------------------------------------------------------------------------
-- VersionRanges normalisation
-------------------------------------------------------------------------------

oldNormaliseVersionRange :: VersionRange -> VersionRange
oldNormaliseVersionRange = Old.fromVersionIntervals . Old.toVersionIntervals

newNormaliseVersionRange :: VersionRange -> VersionRange
newNormaliseVersionRange = New.normaliseVersionRange2

bigVersionRange1 :: IO VersionRange
bigVersionRange1 = either fail return $ eitherParsec
    "(>=1.2.0 && <1.3) || (>=1.3.0 && <1.4) || (>=1.4.0.0 && <1.5) || (>=1.5.0.0 && <1.6)"

bigVersionRange2 :: IO VersionRange
bigVersionRange2 = either fail return $ eitherParsec
    ">=1.2.0 && (<1.3 || >=1.3.0) && (<1.4 || >=1.4.0.0) && (<1.5 || >=1.5.0.0) && <1.6"

bigVersionRange3 :: IO VersionRange
bigVersionRange3 = either fail return $ eitherParsec
    "(>=1.2.0 && <1.3) || (>=1.4.0.0 && <1.5) || (>=1.3.0 && <1.4) || (>=1.5.0.0 && <1.6)"

bigVersionRange4 :: IO VersionRange
bigVersionRange4 = either fail return $ eitherParsec
    ">=1.2.0 && <1.6 && (<1.4 || >=1.4.0.0) && (<1.3 || >=1.3.0) && (<1.5 || >=1.5.0.0)"
