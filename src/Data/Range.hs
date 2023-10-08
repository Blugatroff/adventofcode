module Data.Range (Range(..), new, newOrdered, start, end, inRange, size, contains, intersects, intersection, tryMerge, tryMergeAll, points) where
import Data.List (sortBy)
import Data.Function (on)

-- inclusive start and end
data Range a = Range a a deriving (Eq, Ord)

instance Show a => Show (Range a) where
  show (Range x y) = show x <> ".." <> show y

new :: forall a. Ord a => a -> a -> Range a
new s e | e >= s = Range s e
new s e = Range e s

newOrdered :: forall a. Ord a => a -> a -> Maybe (Range a)
newOrdered s e | e >= s = Just (Range s e)
newOrdered _ _ = Nothing

start :: forall a. Range a -> a
start (Range s _) = s

end :: forall a. Range a -> a
end (Range _ e) = e

inRange :: Ord a => Range a -> a -> Bool
inRange (Range s e) n = n >= s && n <= e

size :: Num a => Range a -> a
size (Range s e) = (+) 1 $ abs $ e - s

contains :: Ord a => Range a -> Range a -> Bool
contains l (Range sr er) = inRange l sr && inRange l er

intersects :: Ord a => Range a -> Range a -> Bool
intersects l@(Range sl el) r@(Range sr er) = inRange r sl || inRange r el || inRange l sr || inRange l er

intersection :: forall a. Ord a => Range a -> Range a -> Maybe (Range a)
intersection a@(Range as ae) b@(Range bs be) | intersects a b = Just $ Range (max as bs) (min ae be)
intersection _ _ = Nothing

tryMerge :: Num a => Ord a => Range a -> Range a -> Either (Range a, Range a) (Range a)
tryMerge l@(Range sl el) (Range sr er) | inRange l sr = Right $ Range sl (max el er)
tryMerge l@(Range sl el) (Range sr er) | inRange l er = Right $ Range (min sl sr) el
tryMerge (Range sl el) r@(Range sr er) | inRange r sl = Right $ Range sr (max el er)
tryMerge (Range sl el) r@(Range sr er) | inRange r el = Right $ Range (min sl sr) er
tryMerge (Range sl el) (Range sr er) | el + 1 == sr = Right $ Range sl er
tryMerge (Range sl el) (Range sr er) | er + 1 == sl = Right $ Range sr el
tryMerge l r = Left (l, r)

tryMergeAll :: Num a => Ord a => [Range a] -> [Range a]
tryMergeAll ranges = f $ sortBy (compare `on` start) ranges
  where
  f [] = []
  f [r] = [r]
  f (a : b : rest) = case tryMerge a b of
    Left (a, b) -> a : f (b : rest)
    Right r -> f $ r : rest

points :: Range Int -> [Int]
points (Range s e) = [s..e]
