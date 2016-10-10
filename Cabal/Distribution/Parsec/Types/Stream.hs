{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Types to be used as source for @parsec@ parsers
module Distribution.Parsec.Types.Stream (
    -- * Section arguments
    sectionArg,
    -- * Field lines
    fieldlinesToLazyByteString,
    ) where

import Prelude ()
import Distribution.Compat.Prelude

--import Data.ByteString (ByteString)
import Distribution.Parsec.Types.Common
import Distribution.Parsec.Types.Field
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Parsec as Parsec

-------------------------------------------------------------------------------
-- Section arguments
-------------------------------------------------------------------------------

positionToSourcePos :: Position -> Parsec.SourcePos
positionToSourcePos = undefined

sectionArg :: Parsec.Parsec [SectionArg Position] u (SectionArg Position)
sectionArg = Parsec.token
    show
    (positionToSourcePos . sectionArgAnn)
    Just

-------------------------------------------------------------------------------
-- Field lines
-------------------------------------------------------------------------------

-- | TODO: this approach loses all source position information
fieldlinesToLazyByteString :: [FieldLine ann] -> BSL.ByteString
fieldlinesToLazyByteString = BSL.fromChunks . concatMap f
  where
    f (FieldLine _ bs) = [bs, "\n"]
