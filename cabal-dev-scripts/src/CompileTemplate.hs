{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Control.Applicative              (many, optional, some, (<|>))
import Control.Monad                    (ap, void)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.State.Strict (State, get, modify', put, runState)
import Data.Char                        (isAlphaNum, isLower, toLower, toUpper)
import Data.Functor.Identity            (Identity (..))
import Data.Semialign                   (alignWith)
import Data.These                       (these)
import Text.Parsec                      (eof, lookAhead, notFollowedBy, parse, satisfy, try)
import Text.Parsec.Char                 (char, space, spaces, string)
import Text.Parsec.String               (Parser)

import qualified Control.Monad.EitherK      as U
import qualified Control.Unification        as U
import qualified Control.Unification.IntVar as U
import qualified Control.Unification.Types  as U

import qualified Data.Map            as M
import qualified Options.Applicative as O

main :: IO ()
main = do
    Opts input output mn <- O.execParser $ O.info (O.helper <*> optsP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Generate template module"
        ]

    contents <- readFile input
    nodes <- either (fail . show) return $
        parse (nodesP <* eof) input contents

    (nodes', rootTy) <- either (fail . show) return $ inference nodes

    let (strTypes, types) = generateTypes rootTy
    let types' = concatMap (\t -> ", " ++ t ++ "(..)") $ reverse types

    let contentsO :: String
        contentsO = unlines
            [ "module " ++ mn ++ " (instantiate" ++ types' ++ ") where"
            , ""
            , strTypes
            , generateTemplate nodes'
            , prelude
            ]

    if output == "-"
    then putStrLn contentsO
    else writeFile output contentsO

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts FilePath FilePath String

optsP :: O.Parser Opts
optsP = Opts
    <$> strArgument
        [ O.metavar "TEMPLATE"
        , O.help    "Template input"
        ]
    <*> strArgument
        [ O.metavar "OUTPUT"
        , O.help    "Haskell module output"
        ]
    <*> strOption
        [ O.long    "module-name"
        , O.metavar "MODULE"
        , O.help    "Module name"
        , O.value   "Main"
        , O.showDefault
        ]
  where
    strArgument = O.strArgument . mconcat
    strOption   = O.strOption   . mconcat

-------------------------------------------------------------------------------
-- Node syntax
-------------------------------------------------------------------------------

type Var = String

type Nodes a = [Node a]

data Node a
    = NRaw  String
    | NExpr (Expr a)
    | NIf   (Expr a) (Nodes a)
    | NFor  Var (Expr a) (Nodes (Maybe a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr a
    = EVar a
    | EField (Expr a) Var
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Expr where
    pure = return
    (<*>) = ap

instance Monad Expr where
    return = EVar

    EVar x          >>= k = k x
    EField expr var >>= k = EField (expr >>= k) var

(>>==) :: Node a -> (a -> Expr b) -> Node b
NRaw s           >>== _ = NRaw s
NExpr expr       >>== k = NExpr (expr >>= k)
NIf expr ns      >>== k = NIf (expr >>= k) (map (>>== k) ns)
NFor var expr ns >>== k = NFor var (expr >>= k) (map (>>== traverse k) ns)

-------------------------------------------------------------------------------
-- "bound"
-------------------------------------------------------------------------------

abstract1 :: (Functor f, Eq a) => a -> f a -> f (Maybe a)
abstract1 x = fmap $ \y ->
    if x == y
    then Nothing
    else (Just y)

instantiate1ret :: Functor f => a -> f (Maybe a) -> f a
instantiate1ret x = fmap (maybe x id)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

varP :: Parser Var
varP = (:) <$> satisfy isLower <*> many (satisfy isVarChar)

isVarChar :: Char -> Bool
isVarChar c = isAlphaNum c || c == '_'

nodeP :: Parser (Node Var)
nodeP = directiveP <|> exprNodeP <|> newlineN <|> rawP

nodesP :: Parser (Nodes Var)
nodesP = many nodeP

newlineN :: Parser (Node Var)
newlineN = NRaw . pure <$> char '\n'

rawP :: Parser (Node Var)
rawP = mk <$> some rawCharP <*> optional (char '\n') where
    rawCharP   = notBrace <|> try (char '{' <* lookAhead notSpecial)
    notBrace   = satisfy $ \c -> c /= '{' && c /= '\n'
    notSpecial = satisfy $ \c -> c /= '{' && c /= '\n' && c /= '%'

    mk s Nothing  = NRaw s
    mk s (Just c) = NRaw (s ++ [c])

exprNodeP :: Parser (Node Var)
exprNodeP = do
    _ <- try (string "{{")
    spaces
    expr <- exprP
    spaces
    _ <- string "}}"
    return (NExpr expr)

exprP :: Parser (Expr Var)
exprP =  do
    v <- varP
    vs <- many (char '.' *> varP)
    return $ foldl EField (EVar v) vs

directiveP :: Parser (Node Var)
directiveP = forP <|> ifP

spaces1 :: Parser ()
spaces1 = space *> spaces

open :: String -> Parser ()
open n = void $ try $ string "{%" *> spaces *> string n *> spaces

close :: String -> Parser ()
close n = do
    open ("end" ++ n)
    _ <- string "%}"
    _ <- many (char ' ')
    eof <|> void (char '\n')

forP :: Parser (Node Var)
forP = do
    open "for"
    var <- varP
    spaces1
    _ <- string "in"
    notFollowedBy $ satisfy isAlphaNum
    spaces1
    expr <- exprP
    spaces1
    _ <- string "%}"
    _ <- optional (char '\n')
    ns <- nodesP
    close "for"
    return $ NFor var expr (abstract1 var <$> ns)

ifP :: Parser (Node Var)
ifP = do
    _ <- open "if"
    expr <- exprP
    spaces
    _ <- string "%}"
    _ <- optional (char '\n')
    ns <- nodesP
    close "if"
    return $ NIf expr ns

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Ty
    = TyUnit
    | TyBool
    | TyString
    | TyList Ty
    | TyRecord (M.Map Var Ty)
  deriving (Eq, Ord, Show)

data TyF a
    = TyVarF a
    | TyUnitF
    | TyBoolF
    | TyStringF
    | TyListF a
    | TyRecordF (M.Map Var a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance U.Unifiable TyF where
    zipMatch (TyVarF a)    (TyVarF b)    = Just (TyVarF (Right (a, b)))
    zipMatch TyUnitF       TyUnitF       = Just TyUnitF
    zipMatch TyBoolF       TyBoolF       = Just TyBoolF
    zipMatch TyStringF     TyStringF     = Just TyStringF
    zipMatch (TyListF a)   (TyListF b)   = Just (TyListF (Right (a, b)))
    zipMatch (TyRecordF a) (TyRecordF b) = Just $ TyRecordF $
        alignWith (these Left Left (curry Right)) a b

    zipMatch _ _ = Nothing

-------------------------------------------------------------------------------
-- Solve constraints
-------------------------------------------------------------------------------

type Error = U.UFailure TyF U.IntVar
type Unify = U.EitherKT Error (U.IntBindingT TyF Identity)
type UTy = U.UTerm TyF U.IntVar

freeze :: UTy -> Ty
freeze (U.UVar _)              = TyUnit
freeze (U.UTerm (TyVarF x))    = freeze x
freeze (U.UTerm TyUnitF)       = TyUnit
freeze (U.UTerm TyBoolF)       = TyBool
freeze (U.UTerm TyStringF)     = TyString
freeze (U.UTerm (TyListF a))   = TyList (freeze a)
freeze (U.UTerm (TyRecordF m)) = TyRecord (fmap freeze m)

inference :: [Node Var] -> Either Error ([Node Var], Ty)
inference ns0 =
    fmap (fmap freeze) $ runIdentity $ U.evalIntBindingT $ U.runEitherKT $ do
        root <- newVar
        let ns1 = fmap (fmap (rootField root)) ns0
        let ns2 = fmap (>>== id) ns1
        ns3 <- inferenceM  ns2
        root' <- U.applyBindings root
        return (ns3, root')
  where
    rootField :: UTy -> Var -> Expr (Var, UTy)
    rootField root field = EField (EVar ("cfg", root)) field

newVar :: Unify UTy
newVar = lift $ U.UVar <$> U.freeVar

inferenceM :: forall a. [Node (a, UTy)] -> Unify [Node a]
inferenceM = traverse go where
    go :: Node (a, UTy) -> Unify (Node a)

    go (NRaw t)  = return $ NRaw t

    go (NExpr expr) = do
        (expr', _) <- checkExpr expr (U.UTerm TyStringF)
        return $ NExpr expr'

    go (NFor var expr ns) = do
        elTy <- newVar
        (expr', _) <- checkExpr expr (U.UTerm (TyListF elTy))
        let fwd :: Maybe (a, UTy) -> (Maybe a, UTy)
            fwd Nothing        = (Nothing, elTy)
            fwd (Just (x, ty)) = (Just x, ty)

        let nsB = fmap2 fwd ns
        nsB' <- inferenceM nsB

        return $ NFor var expr' nsB'

    go (NIf expr ns) = do
        (expr', _) <- checkExpr expr (U.UTerm TyBoolF)
        ns' <- inferenceM ns
        return $ NIf expr' ns'

checkExpr :: Expr (a, UTy) -> UTy -> Unify (Expr a, UTy)
checkExpr (EVar (v, ty)) ty' = do
    ty'' <- U.unify ty ty'
    return (EVar v, ty'')
checkExpr (EField expr field) ty' = do
    (expr', exprTy) <- checkExpr expr $ U.UTerm $ TyRecordF $ M.singleton field ty'
    return (EField expr' field, exprTy)

-------------------------------------------------------------------------------
-- Generate types
-------------------------------------------------------------------------------

generateTypes :: Ty -> (String, [TyName])
generateTypes ty0 =
    ( unlines $ "-- data types" : ls
    , types
    )
  where
    ls :: [String]
    types :: [TyName]
    (ls, (_, types)) = runState loop ([("Cfg", ty0)], [])

    loop :: State ([(String, Ty)], [TyName]) [String]
    loop = do
        x <- get
        case x of
            ([], _) -> return []
            ((name, ty):tys, tys') -> do
                put (tys, tys')
                (++) <$> single name ty <*> loop

    single name (TyRecord m) = do
        modify' $ \(x, y) ->
            ( [ (typeNameRec k, ty) | (k, ty) <- M.toList m ]  ++ x
            , name : y
            )
        return $
            [ ""
            , "data " ++ name ++ " = " ++ name
            ] ++ mapHeadTail ("  { " ++) ("  , " ++)
            [ fieldName name k ++ " :: !" ++ typeName k ty
            | (k, ty) <- M.toList m
            ] ++
            [ "  } deriving (Show)"
            ]
    single name (TyList ty) = single name ty
    single _ _ = return []

fieldName :: TyName -> String -> String
fieldName tyName field = mapFirst toLower tyName ++ mapFirst toUpper field

typeName :: String -> Ty -> TyName
typeName name (TyRecord _) = typeNameRec name
typeName _    TyUnit       = "()"
typeName _    TyBool       = "Bool"
typeName _    TyString     = "String"
typeName name (TyList el)  = "[" ++ typeName name el ++ "]"

typeNameRec :: String -> String
typeNameRec = ("Cfg" ++) . mapFirst toUpper

mapHeadTail :: (a -> a) -> (a -> a) -> [a] -> [a]
mapHeadTail _ _ []     = []
mapHeadTail f g (x:xs) = f x : map g xs

mapFirst f = mapHeadTail f id
mapFirst :: (a -> a) -> [a] -> [a]

-------------------------------------------------------------------------------
-- Generate template
-------------------------------------------------------------------------------

type TyName = String

generateTemplate :: [Node Var] -> String
generateTemplate ns0 = unlines $
    "-- template instantiation" :
    "" :
    "instantiate :: Cfg -> String" :
    "instantiate cfg = ($ []) $ id" :
    strs
  where
    ns1 = fmap2 (\s -> (s, "Cfg")) ns0
    strs = goMany "  " ns1

    goMany :: String -> [Node (Var, TyName)] -> [String]
    goMany pfx = concatMap (go pfx)

    go :: String -> Node (Var, TyName) -> [String]
    go pfx (NRaw  s)          = [pfx ++ ". showString " ++ show s]
    go pfx (NExpr e)          = [pfx ++ ". showString (" ++ fst (showExpr e) ++ ")"]
    go pfx (NIf expr ns)    =
        [ pfx ++ ". ( _ifImpl (" ++ varCode ++ ") $ id"
        ] ++
        goMany ("  " ++ pfx) ns ++
        [ pfx ++ "  )"
        ]
      where
        (varCode, _) = showExpr expr
    go pfx (NFor var expr ns) =
        [ pfx ++ ". _forImpl (" ++ varCode ++ ") (\\" ++ var ++ " -> id"
        ] ++
        goMany ("  " ++ pfx) (map (instantiate1ret (var, varTyName)) ns) ++
        [ pfx ++ "  )"
        ]
      where
        (varCode, varTyName) = showExpr expr

showExpr :: Expr (Var, TyName) -> (String, TyName)
showExpr (EVar varTy)        = varTy
showExpr (EField expr field) =
    (fieldName exprTyName field ++ " $ " ++ exprCode, typeNameRec field)
  where
    (exprCode, exprTyName) = showExpr expr

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | Functions used by genreate template.
--
-- Their names begin with underscores for two reasons:
--
-- * If they are not used, there are no warnings
--
-- * Less likely to clash with some identifier.
--
prelude :: String
prelude = unlines
    [ "-- 'prelude'"
    , ""
    , "_ifImpl :: Bool -> ShowS -> ShowS"
    , "_ifImpl True  s = s"
    , "_ifImpl False _ = id"
    , ""
    , "_forImpl :: [a] -> (a -> ShowS) -> ShowS"
    , "_forImpl xs f = foldr (\\x s -> f x . s) id xs"
    ]
