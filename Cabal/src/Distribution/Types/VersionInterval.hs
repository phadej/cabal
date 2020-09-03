{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | In @Cabal-3.6@ this module have been rewritten.
--
--
module Distribution.Types.VersionInterval (
    -- * Version intervals
    VersionIntervals,
    unVersionIntervals,

    -- * Conversions
    toVersionIntervals,
    fromVersionIntervals,

    -- ** Normalisation
    normaliseVersionRange2,

    -- * Relaxation
    relaxLastInterval,
    relaxHeadInterval,

    -- * Version intervals view
    asVersionIntervals,
    VersionInterval (..),
    LowerBound(..),
    UpperBound(..),
    Bound(..),

    -- * Invariants
    invariantVersionIntervals,
    ) where

import Control.Applicative         (liftA2)
import Control.Exception           (assert)
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal

-- To test this module, and to run version range normalisation benchmarks:
--
-- cabal run Cabal:unit-tests -- -p Distribution.Version
-- cabal run cabal-benchmarks -- -o bench.html normaliseVersionRange

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping,
-- non-empty intervals.
--
-- The key point is that this representation gives a canonical representation
-- for the semantics of 'VersionRange's. This makes it easier to check things
-- like whether a version range is empty, covers all versions, or requires a
-- certain minimum or maximum version. It also makes it easy to check equality
-- or containment. It also makes it easier to identify \'simple\' version
-- predicates for translation into foreign packaging systems that do not
-- support complex version range expressions.
--
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show, Typeable)

-- | Inspect the list of version intervals.
--
unVersionIntervals :: VersionIntervals -> [VersionInterval]
unVersionIntervals (VersionIntervals is) = is

data VersionInterval = VersionInterval !LowerBound !UpperBound    deriving (Eq, Show)
data LowerBound      = LowerBound !Version !Bound                 deriving (Eq, Show)
data UpperBound      = NoUpperBound
                     | UpperBound !Version !Bound                 
                     | MajorBound !Version !UpperBound'           deriving (Eq, Show)
data UpperBound'     = NoUpperBound'
                     | UpperBound' !Version !Bound                deriving (Eq, Show)
data Bound           = ExclusiveBound | InclusiveBound            deriving (Eq, Show)

zeroLowerBound :: LowerBound
zeroLowerBound = LowerBound version0 InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 = (==) version0

-------------------------------------------------------------------------------
-- Stage1
-------------------------------------------------------------------------------

preprocess :: VersionRange -> [VersionInterval]
preprocess = cataVersionRange alg where
    -- version range leafs transform into singleton intervals
    alg (ThisVersionF v)                = [VersionInterval (LowerBound v InclusiveBound) (UpperBound v InclusiveBound)]
    alg (LaterVersionF v)               = [VersionInterval (LowerBound v ExclusiveBound) NoUpperBound]
    alg (OrLaterVersionF v)             = [VersionInterval (LowerBound v InclusiveBound) NoUpperBound]
    alg (EarlierVersionF v)
        | isVersion0 v                  = []
        | otherwise                     = [VersionInterval zeroLowerBound                (UpperBound v ExclusiveBound)]
    alg (OrEarlierVersionF v)           = [VersionInterval zeroLowerBound                (UpperBound v InclusiveBound)]

    -- ^>= version-range's upper bound is MajorBound
    alg (MajorBoundVersionF v)          = [VersionInterval (LowerBound v InclusiveBound) (MajorBound (majorUpperBound v) NoUpperBound')]

    -- union: just merge the version intervals
    alg (UnionVersionRangesF v1 v2)     = checkPreprocessInvariant $
        mergeSortedBy lowerboundLT v1 v2

    -- intersection: pairwise intersect. Strip empty intervals. Sort to restore the invariant.
    alg (IntersectVersionRangesF v1 v2) = checkPreprocessInvariant $
        sortBy lowerboundCmp $ mapMaybe nonEmptyInterval $ liftA2 intersectInterval v1 v2

-- while preprocessing, we only require that intervals are sorted.
preprocessInvariant :: [VersionInterval] -> Bool
preprocessInvariant []        = True
preprocessInvariant xs@(_:ys) = and $ zipWith lowerboundLE xs ys

checkPreprocessInvariant :: [VersionInterval] -> [VersionInterval]
checkPreprocessInvariant xs = assert (preprocessInvariant xs) xs
{-# INLINE checkPreprocessInvariant #-}

lowerboundLE :: VersionInterval -> VersionInterval -> Bool
lowerboundLE x y = lowerboundCmp x y /= GT

lowerboundLT :: VersionInterval -> VersionInterval -> Bool
lowerboundLT x y = lowerboundCmp x y == LT

lowerboundCmp :: VersionInterval -> VersionInterval -> Ordering
lowerboundCmp (VersionInterval (LowerBound v vb) _) (VersionInterval (LowerBound u ub) _) =
    compare v u `mappend` compareBound vb ub
  where
    compareBound :: Bound -> Bound -> Ordering
    compareBound InclusiveBound InclusiveBound = EQ
    compareBound InclusiveBound ExclusiveBound = LT
    compareBound ExclusiveBound InclusiveBound = GT
    compareBound ExclusiveBound ExclusiveBound = EQ

-------------------------------------------------------------------------------
-- Postprocess
-------------------------------------------------------------------------------

-- | Post-processing takes a list of ordered version intervals,
-- but possibly overlapping, and creates 'VersionIntervals'.
--
postprocess :: [VersionInterval] -> VersionIntervals
postprocess []                                     = checkInvariant $ VersionIntervals []
postprocess (VersionInterval lb ub : rest)         = checkInvariant $ VersionIntervals $ postprocess' lb ub rest

postprocess' :: LowerBound -> UpperBound -> [VersionInterval] -> [VersionInterval]
postprocess' !lb NoUpperBound _                                 = [VersionInterval lb NoUpperBound]
postprocess' !lb !ub          []                                = [VersionInterval lb ub]
postprocess' !lb !ub          (VersionInterval lb' ub' : rest')
    | doesNotTouch ub lb'                                       = VersionInterval lb ub : postprocess' lb' ub' rest'
    | otherwise                                                 = postprocess' lb (unionUpper ub ub') rest'

-------------------------------------------------------------------------------
-- Intersections
-------------------------------------------------------------------------------

-- | Create an interval and 
nonEmptyInterval :: VersionInterval -> Maybe VersionInterval
nonEmptyInterval i | nonEmpty i = Just i
nonEmptyInterval _              = Nothing

intersectInterval :: VersionInterval -> VersionInterval -> VersionInterval
intersectInterval (VersionInterval lv uv) (VersionInterval lu uu) =
    VersionInterval (intersectLower lv lu) (intersectUpper uv uu)

intersectLower :: LowerBound -> LowerBound -> LowerBound
intersectLower (LowerBound v vb) (LowerBound u ub) = case compare v u of
    EQ -> LowerBound v (intersectBound vb ub)
    LT -> LowerBound u ub
    GT -> LowerBound v vb

intersectUpper :: UpperBound -> UpperBound -> UpperBound
intersectUpper NoUpperBound      b                 = b
intersectUpper b                 NoUpperBound      = b
intersectUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
    EQ -> UpperBound v (intersectBound vb ub)
    LT -> UpperBound v vb
    GT -> UpperBound u ub
intersectUpper (MajorBound v vb) (MajorBound u ub) = makeMajorBound (min u v) (intersectUpper' vb ub)
intersectUpper (UpperBound v vb) (MajorBound u ub) = makeMajorBound u (intersectUpper' (UpperBound' v vb) ub)
intersectUpper (MajorBound v vb) (UpperBound u ub) = makeMajorBound v (intersectUpper' vb (UpperBound' u ub))

makeMajorBound :: Version -> UpperBound' -> UpperBound
makeMajorBound v NoUpperBound'          = MajorBound v NoUpperBound'
makeMajorBound v ub@(UpperBound' u ub') = case compare v u of
    LT -> MajorBound v ub
    EQ -> UpperBound u ub'
    GT -> UpperBound u ub'

intersectUpper' :: UpperBound' -> UpperBound' -> UpperBound'
intersectUpper' NoUpperBound'      b                 = b
intersectUpper' b                 NoUpperBound'      = b
intersectUpper' (UpperBound' v vb) (UpperBound' u ub) = case compare v u of
    EQ -> UpperBound' v (intersectBound vb ub)
    LT -> UpperBound' v vb
    GT -> UpperBound' u ub

intersectBound :: Bound -> Bound -> Bound
intersectBound InclusiveBound InclusiveBound = InclusiveBound
intersectBound _              _              = ExclusiveBound

-------------------------------------------------------------------------------
-- Unions
-------------------------------------------------------------------------------

unionUpper :: UpperBound -> UpperBound -> UpperBound
unionUpper NoUpperBound      _                 = NoUpperBound
unionUpper _                 NoUpperBound      = NoUpperBound
unionUpper (UpperBound v vb) (UpperBound u ub) = case compare v u of
    EQ -> UpperBound v (unionBound vb ub)
    LT -> UpperBound u ub
    GT -> UpperBound v vb
unionUpper (MajorBound v vb) (MajorBound u ub)  = MajorBound (max u v) (unionUpper' vb ub)
unionUpper (UpperBound v vb) (MajorBound u ub)  = case compare v u of
    LT -> MajorBound u (unionUpper' (UpperBound' v vb) ub)
    _  -> UpperBound v vb
unionUpper (MajorBound v vb) (UpperBound u ub) = case compare v u of
    GT -> UpperBound u ub
    _  -> MajorBound v (unionUpper' vb (UpperBound' u ub))

unionUpper' :: UpperBound' -> UpperBound' -> UpperBound'
unionUpper' NoUpperBound'      _                  = NoUpperBound'
unionUpper' _                 NoUpperBound'       = NoUpperBound'
unionUpper' (UpperBound' v vb) (UpperBound' u ub) = case compare v u of
    EQ -> UpperBound' v (unionBound vb ub)
    LT -> UpperBound' u ub
    GT -> UpperBound' v vb

unionBound :: Bound -> Bound -> Bound
unionBound ExclusiveBound ExclusiveBound = ExclusiveBound
unionBound _              _              = InclusiveBound

-------------------------------------------------------------------------------
-- VersionRange
-------------------------------------------------------------------------------

-- | View a 'VersionRange' as a union of intervals.
--
-- This provides a canonical view of the semantics of a 'VersionRange' as
-- opposed to the syntax of the expression used to define it. For the syntactic
-- view use 'foldVersionRange'.
--
-- Each interval is non-empty. The sequence is in increasing order and no
-- intervals overlap or touch. Therefore only the first and last can be
-- unbounded. The sequence can be empty if the range is empty
-- (e.g. a range expression like @< 1 && > 2@).
--
-- Other checks are trivial to implement using this view. For example:
--
-- > isNoVersion vr | [] <- asVersionIntervals vr = True
-- >                | otherwise                   = False
--
-- > isSpecificVersion vr
-- >    | [(LowerBound v  InclusiveBound
-- >       ,UpperBound v' InclusiveBound)] <- asVersionIntervals vr
-- >    , v == v'   = Just v
-- >    | otherwise = Nothing
--
asVersionIntervals :: VersionRange -> [VersionInterval]
asVersionIntervals = unVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Check an upper bound does not intersect, or even touch a lower bound:
--
-- @
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
-- @
--
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound      _                 = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
    (u < l) || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)
doesNotTouch (MajorBound u _) (LowerBound l lb) =
    (u < l) || (u == l && lb == ExclusiveBound)

-- | Merge sorted list, producing sorted output.
mergeSortedBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeSortedBy le = go where
    go [] ys = ys
    go xs [] = xs
    go xs@(x:xs') ys@(y:ys')
        | le x y    = x : go xs' ys
        | otherwise = y : go xs  ys'


-- | Test if a version falls within the version intervals.
--
-- It exists mostly for completeness and testing. It satisfies the following
-- properties:
--
-- > withinIntervals v (toVersionIntervals vr) = withinRange v vr
-- > withinIntervals v ivs = withinRange v (fromVersionIntervals ivs)
--
-- _withinIntervals :: Version -> VersionIntervals -> Bool
-- _withinIntervals v (VersionIntervals intervals) = any withinInterval intervals
--   where
--     withinInterval (VersionInterval lowerBound upperBound) =
--         withinLower lowerBound && withinUpper upperBound
-- 
--     withinLower (LowerBound v' ExclusiveBound) = v' <  v
--     withinLower (LowerBound v' InclusiveBound) = v' <= v
-- 
--     withinUpper NoUpperBound                   = True
--     withinUpper (UpperBound v' ExclusiveBound) = v' >  v
--     withinUpper (UpperBound v' InclusiveBound) = v' >= v

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

-- | 'VersionIntervals' invariant:
--
-- * all intervals are valid (lower bound is less then upper bound, non-empty)
-- * sorted
-- * intervals doesn't touch each other
--
invariantVersionIntervals :: VersionIntervals -> Bool
invariantVersionIntervals (VersionIntervals intervals) =
    all validInterval intervals &&
    all doesNotTouch' adjacentIntervals &&
    preprocessInvariant intervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' (VersionInterval _ u, VersionInterval l' _) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals = case intervals of
      []     -> []
      (_:tl) -> zip intervals tl

checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariantVersionIntervals is) is
{-# INLINE checkInvariant #-}

validInterval :: VersionInterval -> Bool
validInterval i@(VersionInterval l u) = validLower l && validUpper u && nonEmpty i
  where
    validLower (LowerBound v _) = validVersion v

    validUpper NoUpperBound      = True
    validUpper (UpperBound v _)  = validVersion v
    validUpper (MajorBound v vb) = validVersion v && validUpper' vb

    validUpper' NoUpperBound'     = True
    validUpper' (UpperBound' v _) = validVersion v

-- Check an interval is non-empty
--
nonEmpty :: VersionInterval -> Bool
nonEmpty (VersionInterval _                 NoUpperBound   )   = True
nonEmpty (VersionInterval (LowerBound l lb) (UpperBound u ub)) =
    (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)
nonEmpty (VersionInterval (LowerBound l lb) (MajorBound u _)) =
    (l < u) || (l == u && lb == InclusiveBound)
  where

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert a 'VersionRange' to a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = postprocess . preprocess

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals [])     = noVersion
fromVersionIntervals (VersionIntervals (x:xs)) = foldr1 unionVersionRanges (fmap intervalToVersionRange (x:|xs))

intervalToVersionRange :: VersionInterval -> VersionRange
intervalToVersionRange (VersionInterval (LowerBound v vb) upper') = case upper' of
    NoUpperBound
        -> lowerBound

    UpperBound u ub
        | vb == InclusiveBound
        , ub == InclusiveBound
        , v == u
        -> thisVersion v

    UpperBound u ub -> withLowerBound (makeUpperBound u ub)

    MajorBound u NoUpperBound'
        | majorUpperBound v == u
        , vb == InclusiveBound
        -> majorBoundVersion v

        | majorUpperBound v == u
        -> withLowerBound (majorBoundVersion v)

        | otherwise
        -> withLowerBound (earlierVersion u') `unionVersionRanges` majorBoundVersion u'
      where
        u' = previousMajor u

    MajorBound u (UpperBound' w wb)
        | majorUpperBound v == u
        , vb == InclusiveBound
        -> majorBoundVersion v `intersectVersionRanges` makeUpperBound w wb

        | majorUpperBound v == u
        -> withLowerBound (majorBoundVersion v `intersectVersionRanges` makeUpperBound w wb)

        | otherwise
        -> withLowerBound (earlierVersion u' `intersectVersionRanges` makeUpperBound w wb) `unionVersionRanges` majorBoundVersion u' 
      where
        u' = previousMajor u

  where
    lowerBound :: VersionRange
    lowerBound = case vb of
        InclusiveBound -> orLaterVersion v
        ExclusiveBound -> laterVersion v

    withLowerBound :: VersionRange -> VersionRange
    withLowerBound vr
        | isVersion0 v, vb == InclusiveBound = vr
        | otherwise                          = intersectVersionRanges lowerBound vr

    previousMajor :: Version -> Version
    previousMajor = alterVersion $ \numbers -> case numbers of
        []        -> [0] -- should not happen
        [_]       -> [0] -- e.g. version '1'
        (m1:0:_)  -> [m1]
        (m1:1:_)  -> [m1]
        (m1:m2:_) -> [m1,m2-1]

    makeUpperBound :: Version -> Bound -> VersionRange
    makeUpperBound u InclusiveBound = orEarlierVersion u
    makeUpperBound u ExclusiveBound = earlierVersion u

{-
intervalToVersionRange (VersionInterval (LowerBound v InclusiveBound) (MajorBound u ub))
    | majorUpperBound v == u
    = maybe id (flip intersectVersionRanges) (upperBound' ub) $ majorBoundVersion v
  where
    upperBound' NoUpperBound'                  = Nothing
    upperBound' (UpperBound' v InclusiveBound) = Just (orEarlierVersion v)
    upperBound' (UpperBound' v ExclusiveBound) = Just (earlierVersion v)

intervalToVersionRange (VersionInterval l u) =
    lowerBound l `intersectVersionRanges'` upperBound u
  where


    upperBound NoUpperBound                  = Nothing
    upperBound (UpperBound v InclusiveBound) = Just (orEarlierVersion v)
    upperBound (UpperBound v ExclusiveBound) = Just (earlierVersion v)
    -- TODO
    upperBound (MajorBound v vb)             = Just $ 
        unionVersionRanges (earlierVersion v') $ maybe id (flip intersectVersionRanges) (upperBound' vb) $ majorBoundVersion v'
      where
        v' = previousMajor v

    upperBound' NoUpperBound'                  = Nothing
    upperBound' (UpperBound' v InclusiveBound) = Just (orEarlierVersion v)
    upperBound' (UpperBound' v ExclusiveBound) = Just (earlierVersion v)




-}

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

-- | Since @Cabal-3.6@ this function.. TODO
--
normaliseVersionRange2 :: VersionRange -> VersionRange
normaliseVersionRange2 = fromVersionIntervals . toVersionIntervals

-------------------------------------------------------------------------------
-- Relaxation
-------------------------------------------------------------------------------

relaxLastInterval :: VersionIntervals -> VersionIntervals
relaxLastInterval (VersionIntervals xs) = VersionIntervals (relaxLastInterval' xs)
  where
    relaxLastInterval' []                    = []
    relaxLastInterval' [VersionInterval l _] = [VersionInterval l NoUpperBound]
    relaxLastInterval' (i:is)                = i : relaxLastInterval' is

relaxHeadInterval :: VersionIntervals -> VersionIntervals
relaxHeadInterval (VersionIntervals xs) = VersionIntervals (relaxHeadInterval' xs)
  where
    relaxHeadInterval' []                         = []
    relaxHeadInterval' (VersionInterval _ u : is) = VersionInterval zeroLowerBound u : is
