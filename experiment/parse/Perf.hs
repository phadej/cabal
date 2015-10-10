module Main where

import IndexUtils

import qualified OldParse  (readFields, ParseResult(..))
import Distribution.Simple.Utils (fromUTF8)

import qualified Parser

import System.Environment
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

checkedFiles :: [FilePath]
checkedFiles =
  [
{-
Executable NameServer
  Main-Is:        NameServer.hs
  Other modules:  DSTM, NameService
-}
    "DSTM/0.1/DSTM.cabal"
  , "DSTM/0.1.1/DSTM.cabal"
  , "DSTM/0.1.2/DSTM.cabal"

-- unexpected character in input '\194'
  , "Octree/0.5/Octree.cabal"

{-
 default- extensions:
               ScopedTypeVariables,
-}

  , "control-monad-exception-mtl/0.10.3/control-monad-exception-mtl.cabal"

{-
  default-language:    Haskell2010
Test-Suite test-unify:
  hs-source-dirs: test
-}

  , "ds-kanren/0.2.0.0/ds-kanren.cabal"
  , "ds-kanren/0.2.0.1/ds-kanren.cabal"


-- unexpected character in input '\194'
-- a0 is NBSP
-- 194 160 is the UTF-8 encoding of a NO-BREAK SPACE codepoint (the same codepoint that HTML calls &nbsp;).
-- http://stackoverflow.com/questions/13992934/how-to-fix-utf-encoding-for-whitespaces
{-
000019b0  70 6f 73 69 74 6f 72 79  20 68 65 61 64 0d 0a c2  |pository head...|
000019c0  a0 c2 a0 74 79 70 65 3a  20 20 20 20 20 67 69 74  |...type:     git|
000019d0  0d 0a c2 a0 c2 a0 6c 6f  63 61 74 69 6f 6e 3a 20  |......location: |
-}

  , "hermit/0.1.8.0/hermit.cabal"

-- Using {- as a comment
  , "ixset/1.0.4/ixset.cabal"

-- test-suite metric-tests:
  , "metric/0.1.4/metric.cabal"
  , "metric/0.2.0/metric.cabal"

-- unexpected character in input '\194'
  , "oeis/0.3.0/oeis.cabal"
  , "oeis/0.3.1/oeis.cabal"
  , "oeis/0.3.2/oeis.cabal"
  , "oeis/0.3.3/oeis.cabal"
  , "oeis/0.3.4/oeis.cabal"
  , "oeis/0.3.5/oeis.cabal"
  , "oeis/0.3.6/oeis.cabal"

--     impl(ghc >= 7.4):
  , "phasechange/0.1/phasechange.cabal"
  , "shelltestrunner/1.3/shelltestrunner.cabal"
  , "smartword/0.0.0.5/smartword.cabal"

-- unexpected character in input '\DEL'
  , "vacuum-opengl/0.0/vacuum-opengl.cabal"
  , "vacuum-opengl/0.0.1/vacuum-opengl.cabal"
  ]

main :: IO ()
main = do
  [which, n, indexfile] <- getArgs
  cabalFiles <- IndexUtils.readPackageIndexFile indexfile

  case which of
    "perf-baseline" -> print (length cabalFiles)

    "perf-old" -> let parse  = OldParse.readFields . fromUTF8 . LBS.unpack . snd
                      parsed = [ pkg | OldParse.ParseOk _ pkg <- map parse cabalFiles ]
                  in  print (length parsed)

    "perf-new" -> let parse  = Parser.readFields . toStrict . snd
                      parsed = [ pkg | Right pkg <- map parse cabalFiles ]
                   in  print (length parsed)

    "check-old" -> let parse  = OldParse.readFields . fromUTF8 . LBS.unpack . snd
                       parsed = [ (msg, f)
                                | (OldParse.ParseFailed msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case parsed of
                         []           -> print "all ok!"
                         ((msg, (name, f)):_) -> do print msg
                                                    LBS.putStr f

    "check-new" -> let parse  = Parser.readFields . toStrict . snd
                       parsed = [ (msg, f)
                                | (Left msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case drop (read n) $ filter (\(_, (name, _)) -> name `notElem` checkedFiles) parsed of
                         []           -> print "all ok!"
                         ((msg, (name, f)):_) -> do putStrLn name
                                                    print msg
                                                    LBS.putStr f

    "extract-fail" -> let parse  = Parser.readFields . toStrict . snd
                          parsed = [ f
                                   | (Left msg, f) <-
                                     map (\f -> (parse f, f)) cabalFiles ]
                      in  sequence_
                            [ LBS.writeFile ("fail/" ++ show n ++ ".cabal") (snd f)
                            | (f,n) <- zip parsed [0..] ]

  where
    toStrict = BS.concat . LBS.toChunks