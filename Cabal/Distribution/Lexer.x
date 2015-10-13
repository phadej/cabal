{
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Parsec.Lexer
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Lexer for the cabal files.
{-# LANGUAGE BangPatterns #-}
module Distribution.Lexer
  (ltest, lexToken, Token(..), LToken(..)
  ,bol_section, in_section, in_field_layout, in_field_braces
  ,mkLexState) where

import Distribution.Parsec.LexerMonad
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B.Char8
import Data.Word (Word8)
import qualified Data.Char
import Data.Char (chr, ord)
import Data.List (stripPrefix)
--import Distribution.Simple.Utils (fromUTF8) --don't decode, keep as ByteString

-- testing only:
import Debug.Trace
import Control.Exception (assert)
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
}

-- Various character classes

$space           = \          -- single space char
$digit           = 0-9        -- digits
$alpha           = [a-z A-Z]  -- alphabetic characters
$symbol          = [\= \< \> \+ \* \- \& \| \! \$ \% \^ \@ \# \? \/ \\ \~]
$ctlchar         = [\x0-\x1f \x7f]
$printable       = \x0-\x10ffff # $ctlchar   -- so no \n \r
$spacetab        = [$space \t]
$bom             = \xfeff
$nbsp            = \xa0
$nbspspacetab    = [$nbsp $space \t]

$paren           = [ \( \) \[ \] ]
$field_layout    = [$printable \t]
$field_layout'   = [$printable] # [$space]
$field_braces    = [$printable \t] # [\{ \}]
$field_braces'   = [$printable] # [\{ \} $space]
$comment         = [$printable \t]
$namecore        = [$alpha]
$nameextra       = [$namecore $digit \- \_ \. \']
$instr           = [$printable $space] # [\"]
$instresc        = $printable

@nl          = \n | \r\n | \r
@name        = $nameextra* $namecore $nameextra*
@string      = \" ( $instr | \\ $instresc )* \"
@numlike     = $digit [$digit \.]*
@oplike      = [ \, \. \= \< \> \+ \* \- \& \| \! \$ \% \^ \@ \# \? \/ \\ \~ ]+

tokens :-

<0> {
  $bom   ;
  ()     ;
}

<bol_section, bol_field_layout, bol_field_braces> {
  $spacetab* @nl                        { \_ _ _ -> adjustPos retPos >> lexToken }
  $nbspspacetab+ @nl                    { \_ _ _ -> adjustPos retPos >> addWarning "Non-breaking space occured" >> lexToken }
  -- no @nl here to allow for comments on last line of the file with no trailing \n
  $spacetab* "--" $comment*             ;  -- TODO: check the lack of @nl works here
                                        -- including counting line numbers
}

<bol_section> {
  $spacetab*  --TODO prevent or record leading tabs
                   { \pos len inp -> if B.length inp == len
                                       then return (L pos EOF)
                                       else setStartCode in_section
                                         >> return (L pos (Indent len)) }
  $spacetab* \{    { tok  OpenBrace }
  $spacetab* \}    { tok  CloseBrace }
}

<in_section> {
  $spacetab+   ; --TODO: don't allow tab as leading space

  "--" $comment* ;

  @name        { toki TokSym }
  @string      { \p l i -> case reads (B.Char8.unpack (B.take l i)) of
                             [(str,[])] -> return (L p (TokStr str))
                             _          -> lexicalError p i }
  @numlike     { toki TokNum }
  @oplike      { toki TokOther }
  $paren       { toki TokOther }
  \:           { tok  Colon }
  \{           { tok  OpenBrace }
  \}           { tok  CloseBrace }
  @nl          { \_ _ _ -> adjustPos retPos >> setStartCode bol_section >> lexToken }
}

<bol_field_layout> {
  $spacetab*   --TODO prevent or record leading tabs
                { \pos len inp -> if B.length inp == len
                                    then return (L pos EOF)
                                    else setStartCode in_field_layout
                                      >> return (L pos (Indent len)) }
}

<in_field_layout> {
  $spacetab+; --TODO prevent or record leading tabs
  $field_layout' $field_layout*  { toki TokFieldLine }
  @nl             { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_layout >> lexToken }
}

<bol_field_braces> {
   ()                { \_ _ _ -> setStartCode in_field_braces >> lexToken }
}

<in_field_braces> {
  $spacetab+; --TODO prevent or record leading tabs
  $field_braces' $field_braces*    { toki TokFieldLine }
  \{                { tok  OpenBrace  }
  \}                { tok  CloseBrace }
  @nl               { \_ _ _ -> adjustPos retPos >> setStartCode bol_field_braces >> lexToken }
}

{

data Token = TokSym   !ByteString
           | TokStr   !String
           | TokNum   !ByteString
           | TokOther !ByteString
           | Indent   !Int
           | TokFieldLine !ByteString
           | Colon
           | OpenBrace
           | CloseBrace
           | EOF
           | LexicalError InputStream --TODO: add separate string lexical error
  deriving Show

data LToken = L !Position !Token
  deriving Show

toki t pos len input = return $! L pos (t (B.take len input))
tokl t pos len _input = return $! L pos (t len)
tok  t pos _len _input = return $! L pos t

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = InputStream

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte = B.uncons

lexicalError :: Position -> InputStream -> Lex LToken
lexicalError pos inp = do
  setInput B.empty
  return $! L pos (LexicalError inp)

lexToken :: Lex LToken
lexToken = do
  pos <- getPos
  inp <- getInput
  st  <- getStartCode
  case alexScan inp st of
    AlexEOF -> return (L pos EOF)
    AlexError inp' ->
        let !len_bytes = B.length inp - B.length inp' in
            --FIXME: we want len_chars here really
            -- need to decode utf8 up to this point
        lexicalError (incPos len_bytes pos) inp'
    AlexSkip  inp' len_chars -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        lexToken
    AlexToken inp' len_chars action -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        let !len_bytes = B.length inp - B.length inp'
        t <- action pos len_bytes inp
        --traceShow t $ return t
        return t

checkPosition pos@(Position lineno colno) inp inp' len_chars = do
    text_lines <- getDbgText
    let len_bytes = B.length inp - B.length inp'
        pos_txt   | lineno-1 < V.length text_lines = T.take len_chars (T.drop (colno-1) (text_lines V.! (lineno-1)))
                  | otherwise = T.empty
        real_txt  = B.take len_bytes inp
    when (pos_txt /= T.decodeUtf8 real_txt) $
      traceShow (pos, pos_txt, T.decodeUtf8 real_txt) $
      traceShow (take 3 (V.toList text_lines)) $ return ()
  where
    getDbgText = Lex $ \s@LexState{ dbgText = txt } -> LexResult s txt

lexAll :: Lex [LToken]
lexAll = do
  t <- lexToken
  case t of
    L _ EOF -> return [t]
    _       -> do ts <- lexAll
                  return (t : ts)

ltest :: Int -> String -> IO ()
ltest code s =
  let xs = execLexer (setStartCode code >> lexAll) (B.Char8.pack s)
   in mapM_ print xs


mkLexState :: B.ByteString -> LexState
mkLexState input = LexState {
                     curPos   = Position 1 1,
                     curInput = input,
                     curCode  = bol_section,
                     warnings = [],
                     dbgText  = V.fromList . lines' . T.decodeUtf8With T.lenientDecode $ input
                   }

lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
                  (l, s2) | Just (c,s3) <- T.uncons s2
                         -> case T.uncons s3 of
                              Just ('\n', s4) | c == '\r' -> l `T.snoc` '\r' `T.snoc` '\n' : lines' s4
                              _                           -> l `T.snoc` c : lines' s3

                          | otherwise
                         -> [l]