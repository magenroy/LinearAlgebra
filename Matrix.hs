module Matrix where
-- should write the index invariant mappings in terms of the index dependent
-- ones

import Control.Applicative (liftA2)
import Data.Array -- don't forget to generalize for more arrays!
import Data.Tuple (swap)
import Data.List (groupBy)
import qualified Data.Foldable as Fold
import Data.Traversable
import Data.Monoid
import Data.Ratio (numerator, denominator)

import Prelude hiding ((!!))
import Data.List (genericIndex)
(!!) = genericIndex

type Index = Word
type Indices = (Index, Index)
type IndexMap a = Index -> Index -> a

newtype Matrix a = Matrix {toArray :: (Array (Index,Index) a)} deriving Eq
newtype Row a = Row [a] deriving Eq
newtype Column a = Column [a] deriving Eq

instance Functor Matrix where
    fmap f (Matrix a) = Matrix $ fmap f a

instance Applicative Matrix where
    pure = Matrix . listArray ((1,1),(1,1)) . pure
    -- <*> could be defined elementwise, or by all the combinations
    (<*>) fs = mapWithIndices (uncurry $ entry fs)

instance Num a => Num (Matrix a) where
    (+) = liftA2 (+)
    (*) = mmult
    (-) = liftA2 (-)
    abs = fmap abs -- or should this be "pure . det"?
    fromInteger = zero . fromInteger
    signum = fmap signum

instance (Fractional a, Eq a) => Fractional (Matrix a) where
    recip m = case invert m of
                Left _ -> recip 0
                Right r -> r
    fromRational r = let (p,q) = (numerator r, denominator r) in fromInteger p / fromInteger q

instance Functor Row where
    fmap f (Row r) = Row $ fmap f r

instance Functor Column where
    fmap f (Column c) = Column $ fmap f c

instance Show a => Show (Matrix a) where
    show = unlines . map (unwords . map show) . toList

instance Show a => Show (Row a) where
    show (Row a) = unwords . map show $ a

instance Show a => Show (Column a) where
    show (Column a) = unlines . map show $ a

instance Fold.Foldable Row where
    foldMap f (Row r) = Fold.foldMap f r

instance Fold.Foldable Column where
    foldMap f (Column c) = Fold.foldMap f c

instance Traversable Row where
    sequenceA (Row r) = fmap Row $ sequenceA r

instance Traversable Column where
    sequenceA (Column c) = fmap Column $ sequenceA c


-- auxiliary function that should exist already (do not export)
mapWithIndex :: (Index -> a -> b) -> [a] -> [b]
mapWithIndex f xs = go xs 0
    where go [] _ = []
          go (x:xs) n = (f n x):go xs (succ n)

toList :: Matrix a -> [[a]]
toList (Matrix a) = map (map snd) . groupBy (\((a,_),_) ((b,_),_) -> a == b) . assocs $ a

toRowCol :: Matrix a -> Row (Column a)
toRowCol = Row . map Column . toList . transpose

toColRow :: Matrix a -> Column (Row a)
toColRow = Column . map Row . toList

dim :: Matrix a -> (Index, Index)
dim (Matrix a) = snd . bounds $ a

width :: Matrix a -> Index
width = snd . dim

height :: Matrix a -> Index
height = fst . dim

empty :: Matrix a
empty = Matrix $ array ((1,1),(0,0)) []

entry :: Matrix a -> IndexMap a
entry (Matrix m) i j = m ! (i,j)

adjust :: [(Indices, a -> a)] -> Matrix a -> Matrix a
adjust xs (Matrix arr) = Matrix $ arr // [(e, f $ arr ! e) | (e,f) <- xs]

mapWithIndices :: (Indices -> a -> b) -> Matrix a -> Matrix b
mapWithIndices f m = fromFunction (dim m) (\i j -> f (i,j) (entry m i j))

--mapRowWithIndex :: (Index -> a -> b) -> Row a -> Row b
--mapRowWithIndex f (Row r) = Row $ mapWithIndex f r
---- I don't like this boilerplate
--mapColumnWithIndex :: (Index -> a -> b) -> Column a -> Column b
--mapColumnWithIndex f (Column c) = Column $ mapWithIndex f c

modify :: (Indices -> Indices) -> Indices -> Matrix a -> Matrix a
modify f dims (Matrix a) = Matrix $ ixmap ((1,1),dims) f a

relocate :: (Indices -> Indices) -> Matrix a -> Matrix a
relocate f m = modify f (dim m) m

transform :: (Indices -> Indices) -> Matrix a -> Matrix a
transform f m = modify f (f $ dim m) m -- cannot perform all kinds of transformations (eg rotation)

transpose :: Matrix a -> Matrix a
transpose = transform swap

subMatrix :: Indices -> Indices -> Matrix a -> Matrix a
subMatrix (i1,j1) (i2,j2) = modify (\(i,j) -> (i + pred i1, j + pred j1)) (succ i2 - i1, succ j2 - j1)

row :: Index -> Matrix a -> Row a
row i m@(Matrix a) = let (Matrix a) = subMatrix (i,1) (i,width m) m in Row $ elems a --ixmap (1, width m) ((,) i) a --- ###########

column :: Index -> Matrix a -> Column a
column i m@(Matrix a) = Column . elems $ ixmap (1, height m) (flip (,) i) a --- ###########

fromFunction :: Indices -> IndexMap a -> Matrix a
fromFunction dims f = let bnds = ((1,1),dims) in Matrix $ array bnds (map (liftA2 (,) id $ uncurry f) $ range bnds)

fromAssoc :: Indices -> [((Index,Index),a)] -> Matrix a
fromAssoc = (Matrix .) . array . ((,) (1,1))

fromList :: Indices -> [[a]] -> Matrix a
fromList dims xs = fromFunction dims (\i j -> xs !! pred i !! pred j) -- very unoptimized (has to traverse list each time (i + j))

-- O(n^3)
mmult :: Num a => Matrix a -> Matrix a -> Matrix a
mmult a b = fromFunction (height a, width b) (\i j -> let
    (Row r) = row i a
    (Column c) = column j b
    in sum $ zipWith (*) r c)

scMult :: Num a => a -> Matrix a -> Matrix a
scMult = fmap . (*)

augmentV :: Matrix a -> Matrix a -> Matrix a
augmentV ma@(Matrix a) mb@(Matrix b) = Matrix $ listArray ((1,1),(height ma + height mb, min (width ma) (width mb))) (elems a ++ elems b)

augmentH :: Matrix a -> Matrix a -> Matrix a
augmentH mat1 mat2 = let ((m1,n1),(m2,n2)) = (dim mat1, dim mat2) in
    fromFunction (min m1 m2, n1 + n2) (\i j -> if j > n1 then entry mat2 i (j - n1) else entry mat1 i j)

delRows :: Index -> Index -> Matrix a -> Matrix a
delRows i1 i2 mat = let dims@(_,n) = dim mat in augmentV (subMatrix (1,1) (i1 - 1, n) mat) (subMatrix (i2 + 1, 1) dims mat)

delRow :: Index -> Matrix a -> Matrix a
delRow i = delRows i i

delCols :: Index -> Index -> Matrix a -> Matrix a
delCols j1 j2 mat = let dims@(m,_) = dim mat in augmentH (subMatrix (1,1) (m, j1 - 1) mat) (subMatrix (1, j2 + 1) dims mat)

delCol :: Index -> Matrix a -> Matrix a
delCol j = delCols j j


insertRow :: Row a -> Index -> Matrix a -> Matrix a
-- not optimal: if we know we are going to access all elements of the row, it will take n(n + 1)/2 instead of n
insertRow (Row r) i mat = let (m,n) = dim mat in fromFunction (succ m,n) inserted
    where inserted i'
            | i' < i = entry mat i'
            | i' == i = (r !!) . pred
            | i' > i = entry mat $ pred i'

insertCol :: Column a -> Index -> Matrix a -> Matrix a
-- not optimal: if we know we are going to access all elements of the row, it will take n(n + 1)/2 instead of n
-- didn't fmap tuple for symmetry with row implementation
insertCol (Column c) j mat = let (m,n) = dim mat in fromFunction (m,succ n) $ flip inserted
    where inserted j'
            | j' < j = flip (entry mat) j'
            | j' == j = (c !!) . pred
            | j' > j = flip (entry mat) $ pred j'

insertRows :: [Row a] -> Index -> Matrix a -> Matrix a -- inserting in multiple places is messy
insertRows [] _ = id
insertRows (r:rs) i = insertRows rs (succ i) . insertRow r i

insertCols :: [Column a] -> Index -> Matrix a -> Matrix a -- inserting in multiple places is messy
insertCols [] _ = id
insertCols (c:cs) i = insertCols cs (succ i) . insertCol c i

applySubMatrix :: (Matrix a -> Matrix a) -> Indices -> Indices -> Matrix a -> Matrix a
applySubMatrix f tl@(t,l) br m = let sub = f $ subMatrix tl br m in
    fromFunction (dim m) (\i j -> if inRange (tl, br) (i,j) then entry sub (i - t + 1) (j - l + 1) else entry m i j)

mapSubMatrix :: (a -> a) -> Indices -> Indices -> Matrix a -> Matrix a
mapSubMatrix = applySubMatrix . fmap

mapSubMatrixWithIndices :: (Indices -> a -> a) -> Indices -> Indices -> Matrix a -> Matrix a
mapSubMatrixWithIndices = applySubMatrix . mapWithIndices

mapNotSubMatrix :: (a -> a) -> Indices -> Indices -> Matrix a -> Matrix a
mapNotSubMatrix f tl br m = let m' = fmap f m in
    fromFunction (dim m) (\i j -> if inRange (tl,br) (i,j) then entry m i j else entry m' i j)

mapNotSubMatrixWithIndices :: (Indices -> a -> a) -> Indices -> Indices -> Matrix a -> Matrix a
mapNotSubMatrixWithIndices f tl br m = let m' = mapWithIndices f m in
    fromFunction (dim m) (\i j -> if inRange (tl,br) (i,j) then entry m i j else entry m' i j)

-- SO MUCH BOILERPLATE
mapRows :: (a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapRows f i1 i2 m = mapSubMatrix f (i1, 1) (i2, width m) m

mapRowsWithIndices :: (Indices -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapRowsWithIndices f i1 i2 m = mapSubMatrixWithIndices f (i1, 1) (i2, width m) m

mapRowsWithIndex :: (Index -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapRowsWithIndex f = mapRowsWithIndices (f . snd)

mapNotRows :: (a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotRows f i1 i2 m = mapNotSubMatrix f (i1, 1) (i2, width m) m

mapNotRowsWithIndices :: (Indices -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotRowsWithIndices f i1 i2 m = mapNotSubMatrixWithIndices f (i1, 1) (i2, width m) m

mapNotRowsWithIndex :: (Index -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotRowsWithIndex f = mapNotRowsWithIndices (f . snd)

mapRow :: (a -> a) -> Index -> Matrix a -> Matrix a
mapRow f i = mapRows f i i

mapRowWithIndex :: (Index -> a -> a) -> Index -> Matrix a -> Matrix a
mapRowWithIndex f i = mapRowsWithIndex f i i

mapNotRow :: (a -> a) -> Index -> Matrix a -> Matrix a
mapNotRow f i = mapNotRows f i i

mapNotRowWithIndices :: (Indices -> a -> a) -> Index -> Matrix a -> Matrix a
mapNotRowWithIndices f i = mapNotRowsWithIndices f i i

mapNotRowWithIndex :: (Index -> a -> a) -> Index -> Matrix a -> Matrix a
mapNotRowWithIndex f i = mapNotRowsWithIndex f i i

mapCols :: (a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapCols f j1 j2 m = mapSubMatrix f (1, j1) (height m, j2) m

mapColsWithIndices :: (Indices -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapColsWithIndices f j1 j2 m = mapSubMatrixWithIndices f (1, j1) (height m, j2) m

mapColsWithIndex :: (Index -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapColsWithIndex f = mapColsWithIndices (f . fst)

mapNotCols :: (a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotCols f j1 j2 m = mapNotSubMatrix f (1, j1) (height m, j2) m

mapNotColsWithIndices :: (Indices -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotColsWithIndices f j1 j2 m = mapNotSubMatrixWithIndices f (1, j1) (height m, j2) m

mapNotColsWithIndex :: (Index -> a -> a) -> Index -> Index -> Matrix a -> Matrix a
mapNotColsWithIndex f = mapNotColsWithIndices (f . fst)

mapCol :: (a -> a) -> Index -> Matrix a -> Matrix a
mapCol f j = mapCols f j j

mapColWithIndex :: (Index -> a -> a) -> Index -> Matrix a -> Matrix a
mapColWithIndex f j = mapColsWithIndex f j j

mapNotCol :: (a -> a) -> Index -> Matrix a -> Matrix a
mapNotCol f j = mapNotCols f j j

mapNotColWithIndices :: (Indices -> a -> a) -> Index -> Matrix a -> Matrix a
mapNotColWithIndices f j = mapNotColsWithIndices f j j

mapNotColWithIndex :: (Index -> a -> a) -> Index -> Matrix a -> Matrix a
mapNotColWithIndex f j = mapNotColsWithIndex f j j


--should be able to move rows, swap multiple rows (eg switch the 2nd and 3rd
--rows with the 4th to 6th rows, etc

switchRows :: Index -> Index -> Matrix a -> Matrix a
switchRows i1 i2 m = fromFunction (dim m) (entry m . interchange)
    where interchange i
            | i == i1 = i2
            | i == i2 = i1
            | otherwise = i

switchCols :: Index -> Index -> Matrix a -> Matrix a
switchCols j1 j2 m = fromFunction (dim m) $ flip (flip (entry m) . interchange)
    where interchange j
            | j == j1 = j2
            | j == j2 = j1
            | otherwise = j

multRow :: Num a => Index -> a -> Matrix a -> Matrix a
multRow i c = mapRow (*c) i

multCol :: Num a => Index -> a -> Matrix a -> Matrix a
multCol j c = mapCol (*c) j

addRows :: Num a => Index -> Index -> a -> Matrix a -> Matrix a
addRows i1 i2 c m = mapRowWithIndex (\j -> (+(c*(entry m i2 j)))) i1 m

makePivotH :: Fractional a => Matrix a -> Index -> Index -> Matrix a
makePivotH m i j = let m' = multRow i (recip $ entry m i j) m
                   in mapNotRowWithIndices (\(i', j') a -> a - (entry m' i' j) * (entry m' i j')) i m'

makePivotV :: Fractional a => Matrix a -> Index -> Index -> Matrix a
makePivotV m i j = let m' = multCol j (recip $ entry m i j) m
                   in mapNotColWithIndices (\(i', j') a -> a - (entry m' i j') * (entry m' i' j)) j m'


-- it would be good to be able to remove the Eq constraint
rowReduce :: (Fractional a, Eq a) => Indices -> Indices -> Matrix a -> Matrix a
rowReduce (i,j) (h,w) m | j == w = m
                        | otherwise = rowReduce (i, succ j) (h,w) m'
                        where m' = case filter (\i' -> entry m i' j /= 0) [j..h] of
                                    [] -> m
                                    (i':_) -> switchRows i' j $ makePivotH m i' j

columnReduce :: (Fractional a, Eq a) => Indices -> Indices -> Matrix a -> Matrix a
columnReduce (i,j) (h,w) m | i == h = m
                           | otherwise = columnReduce (succ i, j) (h,w) m'
                           where m' = case filter (\j' -> entry m i j' /= 0) [i..w] of
                                        [] -> m
                                        (j':_) -> switchCols i j' $ makePivotV m i j'

rref, rcef, ref :: (Fractional a, Eq a) => Matrix a -> Matrix a
rref m = let (h,w) = dim m in rowReduce (1,1) (h, succ w) m
rcef m = let (h,w) = dim m in columnReduce (1,1) (succ h, w) m
ref m | h <= w = rref m
      | otherwise = rcef m
      where (h,w) = dim m

-- simplify this
invert :: (Fractional a, Eq a) => Matrix a -> Either (Matrix a) (Matrix a)
invert m = f $ dir m'
    where dims@(h,w) = dim m
          (m',dir) | h <= w = (rowReduce (1,1) (h, succ w) $ augmentH m (identity h), delCols 1 w)
                   | otherwise = (columnReduce (1,1) (succ h, w) $ augmentV m (identity w), delRows 1 h)
          f | h == w && trace (fmap Product m') /= 0 = Right
            | otherwise = Left

trace :: Monoid m => Matrix m -> m
trace m = let n = uncurry min $ dim m in mconcat [entry m i i | i <- [1..n]]

det :: (Fractional a, Eq a) => Matrix a -> a
det = getProduct . trace . fmap Product . ref -- could be made faster (don't need to fully reduce, only need to make triangular)

--kernel :: (Fractional a, Eq a) => Matrix a ->

--------------------------------------------

kronecker :: Num a => Index -> Index -> a
kronecker i j
    | i == j = 1
    | otherwise = 0

identity :: Num a => Index -> Matrix a
identity n = fromFunction (n,n) kronecker

zero :: Num a => Index -> Matrix a
zero n = fromFunction (n,n) $ const . const 0


-------------------------------
tstmatrix h w init = fromList (h,w) [[i..i + w] | i <- [init,(w + init)..(h * w + init)]]

ftstmatrix h w = fmap toRational . tstmatrix h w
