-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Parsec.LexerMonad
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Parsec.LexerMonad (
    InputStream,
    LexState(..),
    LexResult(..),
    Position(..),
    incPos,
    retPos,

    Lex(..),
    execLexer,

    getPos,
    setPos,
    adjustPos,

    getInput,
    setInput,

    getStartCode,
    setStartCode,

    LexWarning(..),
    addWarning,

  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..))
import Data.Monoid ((<>))
#endif

import Control.Monad (ap, liftM)

import qualified Data.ByteString as B


#ifdef CABAL_PARSEC_DEBUG
-- testing only:
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
#endif

-- simple state monad
newtype Lex a = Lex { unLex :: LexState -> LexResult a }

instance Functor Lex where
  fmap = liftM

instance Applicative Lex where
  pure = returnLex
  (<*>) = ap

instance Monad Lex where
  return = pure
  (>>=)  = thenLex

data LexResult a = LexResult {-# UNPACK #-} !LexState a

data LexWarning = LexWarning {-# UNPACK #-} !Position
                                            !String
  deriving (Show)

data LexState = LexState {
        curPos   :: {-# UNPACK #-} !Position,        -- position at current input location
        curInput :: {-# UNPACK #-} !InputStream,     -- the current input
        curCode  :: {-# UNPACK #-} !StartCode,       -- lexer code
        warnings :: [LexWarning]
#ifdef CABAL_PARSEC_DEBUG
        , dbgText  :: V.Vector T.Text
#endif
     } --TODO: check if we should cache the first token
       -- since it looks like parsec's uncons can be called many times on the same input

type StartCode   = Int    -- ^ An @alex@ lexer start code
type InputStream = B.ByteString

data Position = Position {-# UNPACK #-}   !Int           -- row
                         {-# UNPACK #-}   !Int           -- column
  deriving (Eq, Show)

incPos :: Int -> Position -> Position
incPos n (Position row col) = Position row (col + n)

retPos :: Position -> Position
retPos (Position row _col) = Position (row + 1) 1


-- | Execute the given lexer on the supplied input stream.
execLexer :: Lex a -> InputStream -> ([LexWarning], a)
execLexer (Lex lexer) input =
    case lexer initialState of
      LexResult LexState{ warnings = ws } result -> (ws, result)
  where
    initialState = LexState
      { curPos   = Position 1 1
      , curInput = input
      , curCode  = 0
      , warnings = []
#ifdef CABAL_PARSEC_DEBUG
      , dbgText  = V.fromList . T.lines . T.decodeUtf8 $ input
#endif
      }

{-# INLINE returnLex #-}
returnLex :: a -> Lex a
returnLex a = Lex $ \s -> LexResult s a

{-# INLINE thenLex #-}
thenLex :: Lex a -> (a -> Lex b) -> Lex b
(Lex m) `thenLex` k = Lex $ \s -> case m s of LexResult s' a -> (unLex (k a)) s'

setPos :: Position -> Lex ()
setPos pos = Lex $ \s -> LexResult s{ curPos = pos } ()

getPos :: Lex Position
getPos = Lex $ \s@LexState{ curPos = pos } -> LexResult s pos

adjustPos :: (Position -> Position) -> Lex ()
adjustPos f = Lex $ \s@LexState{ curPos = pos } -> LexResult s{ curPos = f pos } ()

getInput :: Lex InputStream
getInput = Lex $ \s@LexState{ curInput = i } -> LexResult s i

setInput :: InputStream -> Lex ()
setInput i = Lex $ \s -> LexResult s{ curInput = i } ()

getStartCode :: Lex Int
getStartCode = Lex $ \s@LexState{ curCode = c } -> LexResult s c

setStartCode :: Int -> Lex ()
setStartCode c = Lex $ \s -> LexResult s{ curCode = c } ()

-- | Add warning at the current position
addWarning :: String -> Lex ()
addWarning msg = Lex $ \s@LexState{ curPos = pos, warnings = ws  } -> LexResult s{ warnings = LexWarning pos msg : ws }  ()