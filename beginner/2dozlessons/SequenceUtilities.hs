-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 16 - Modules as Libraries
--      provided in text appendix

> module SequenceUtilities
>   (allEqual,
>    apply,
>    blocks,
>    blocksRigid,
>    centerInField,
>    concatWithSpacer,
>    decreasing,
>    decreasingStrictly,
>    dropFromRight,
>    dropWhileFromRight,
>    increasing,
>    increasingStrictly,
>    indicesOfOccurence,
>    leftJustifyWith,
>    monotonic,
>    multiplex,
>    packets,
>    pam,
>    prefixes,
>    quicksort,
>    quicksortWith,
>    reps,
>    rightJustifyWith,
>    splitFromRight,
>    suffixes,
>    takeFromRight,
>    takeUntil,
>    takeWhileFromRight,
>    transpose)
>   where 

group elements of sequence in blocks of given size
Note: For any sequence xs, n::Int with n > 0,
      concat(blocks n xs) = xs

>   blocks :: Int -> [a] -> [ [a] ]
>   blocks blockSize =
>     takeWhile(not . null) . map fst .
>       iterate(splitAt blockSize . snd) . splitAt blockSize


group elements of sequence in blocks of given size
pad last group if necessary to make it the right length

>   blocksRigid :: Int -> a -> [a] -> [ [a] ]
>   blocksRigid blockSize pad =
>     map(leftJustifyWith pad blockSize) . blocks blockSize


package sequence into subsequences terminated by delimiter;
if xs is x1 ++ [d1] ++ x2 ++ [d2] ++  ... ++ xn ++ [dn] or
if xs is x1 ++ [d1] ++ x2 ++ [d2] ++  ... ++ xn
                               where each d satisfies (isDelimiter d),
                               and no element e of any x-i satisifies
                                   (isDelimiter e)
then (packets xs) is [x1, x2, ..., xn]

>   packets :: (a -> Bool) -> [a] -> [[a]]
>   packets isDelimiter =
>     map fst . takeWhile(not . and . map null . pam[fst, snd]) .
>       iterate(break isDelimiter . drop 1 . snd) . break isDelimiter


multiplex a sequence of streams into one stream
using round-robin alternation among streams with elements remaining
Note: if s and t are different elements of the argument of multiplex
      and length(s) >= length(t), then the delivered sequence contains
      an element from s between each succesive element from t
Example: multiplex["abc", "12345", "wxyz"] = "a1wb2xc3y4z5"

>   multiplex :: [[a]] -> [a]
>   multiplex = concat . foldr multiInsert [ ] 


insert elements of the first argument as initial elements of the
sequences in the second argument

>   multiInsert :: [a] -> [[a]] -> [[a]]
>   multiInsert xs yss = matchingPairs ++ tailOfLongerOne
>     where
>     matchingPairs = zipWith (:)  xs yss
>     tailOfLongerOne = (map(:[ ]) . drop n) xs  ++  drop n yss
>     n = length matchingPairs


prefixes delivers all of the non-empty prefixes of its argument:
   prefixes [x1, x2, x3, ...] = [[x1], [x1, x2], [x1, x2, x3], ... ]

>   prefixes :: [a] -> [[a]]
>   prefixes  = drop 1 . scanl (++) [ ] . map(:[ ])


suffixes delivers all of the non-empty suffixes of its argument:
   suffixes [x1, x2, x3, ...] = [[x1, x2, x3, ...],
                                     [x2, x3, ...],
                                         [x3, ...],
                                              ...  ]

>   suffixes :: [a] -> [[a]]
>   suffixes = takeWhile(not . null) . iterate(drop 1)


find indices in a sequence where an item occurs

>   indicesOfOccurence :: Eq a => a -> [a] -> [Int]
>   indicesOfOccurence item items =
>     foldr addIndex [] (zip items [0..])
>     where
>     addIndex (x,index) indexes 
>       | x == item   = [index] ++ indexes
>       | otherwise   = indexes


justify a sequence in a field of a given width
(deliver original sequence if given field-width is too narrow)

>   leftJustifyWith, rightJustifyWith, centerInField ::
>      a -> Int -> [a] -> [a]
>   leftJustifyWith pad fieldWidth xs =
>     xs ++ reps (max 0 (fieldWidth - length xs)) pad
>   rightJustifyWith pad fieldWidth xs =
>     reps (max 0 (fieldWidth - length xs)) pad ++ xs
>   centerInField pad width xs =
>     reps leftPadLength pad ++ xs ++ reps rightPadLength pad
>     where
>     leftPadLength  = max 0 ((width - lengthOfSequence) `div` 2)
>     rightPadLength
>       = max 0 (width - (leftPadLength + lengthOfSequence))
>     lengthOfSequence = length xs


form a sequence consisting of n copies of a given element

>   reps :: Int -> a -> [a]
>   reps n = take n . repeat


shortest prefix of a sequence containing an element
that satisfies a given predicate

>   takeUntil :: (a -> Bool) -> [a] -> [a]
>   takeUntil predicate xs = prePredicate ++ take 1 others
>     where
>     (prePredicate, others) = break predicate xs


from-the-right versions of take, drop, and split

>   takeFromRight, dropFromRight :: Int -> [a] -> [a]
>   takeWhileFromRight, dropWhileFromRight ::
>     (a -> Bool) -> [a] -> [a]
>   splitFromRight :: Int -> [a] -> ([a], [a])
>   takeFromRight  n xs = drop (max 0 (length xs - n)) xs
>   dropFromRight  n xs = take (max 0 (length xs - n)) xs
>   splitFromRight n xs = splitAt (max 0 (length xs - n)) xs
>   takeWhileFromRight p = reverse . takeWhile p . reverse
>   dropWhileFromRight p = reverse . dropWhile p . reverse


concatenate, but include a standard element between appendees
Note: if ws::[String], then concatWithSpacer " " ws = unwords ws

>   concatWithSpacer :: [a] -> [[a]] -> [a]
>   concatWithSpacer spacer [ ] = [ ]
>   concatWithSpacer spacer nonEmptyList@(x : xs) =
>     foldr1 insertSpacer nonEmptyList
>     where
>     insertSpacer x1 x2 = x1 ++ spacer ++ x2


apply a function to an argument

>   apply :: (a -> b) -> a -> b
>   apply f x = f x


dual of map: apply sequence of functions to argument

>   pam :: [a -> b] -> a -> [b]
>   pam fs x = zipWith apply fs (repeat x)


arrange sequence elements in increasing order

>   quicksort :: Ord a => [a] -> [a]
>   quicksort  (firstx : xs) =
>     quicksort[x | x <- xs, x < firstx] ++ [firstx] ++
>     quicksort[x | x <- xs, not(x < firstx)]
>   quicksort [ ] = [ ]


arrange sequence elements in order according to given ordering

>   quicksortWith :: (a -> a -> Bool) -> [a] -> [a]
>   quicksortWith  precedes (firstx : xs) =
>     quicksortWith precedes [x | x <- xs, precedes x  firstx] ++
>     [firstx] ++
>     quicksortWith precedes [x | x <- xs, not(precedes x  firstx)]
>   quicksortWith precedes [ ] = [ ]


check to see if a sequence is monotonic wrt a given transitive relation

>   monotonic :: (a -> a -> Bool) -> [a] -> Bool
>   monotonic precedes xs = (and . zipWith precedes xs . drop 1) xs


check to see if a sequence is increasing, decreasing, or flat

>   allEqual :: Eq a => [a] -> Bool
>   increasing, increasingStrictly,
>     decreasing, decreasingStrictly :: Ord a => [a] -> Bool
>   allEqual = monotonic(==)
>   increasing = monotonic(<=)
>   increasingStrictly = monotonic(<)
>   decreasing = monotonic(>=)
>   decreasingStrictly = monotonic(>)


interchange rows and columns in a column of rows;
the i-th element of the j-th sequence of the delivered result
is the j-th element of the i-th sequence of the argument
Note: successive rows may decrease in length;
      that is, transpose works properly on upper-triangular matrices

>   transpose :: [[a]] -> [[a]]
>   transpose = foldr patchRowAcrossColumns [ ]
>     where
>     patchRowAcrossColumns row columns =
>       zipWith (:) row (columns ++ repeat [ ])

end of SequenceUtilities module
