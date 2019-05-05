-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList,
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond,
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate,
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence,
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse,
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal

-- this function splits a list x with the respect of the monotony of the allSubLists
--this function should be called split (take 2 x) (drop 2 x) in order to split the list x
split :: Ord a => [a] -> [a] -> [[a]]
split [] [] = []
split [] x = [x]
split x [] = [x]
split [a] x = split ([a] ++ [head x]) (tail x)
split sorted unsorted@(c:ys)  | ((a<b) && (b<c)) || ((a>b) && (b>c)) || ((a==b) && (b==c)) = split (sorted ++ [c]) ys
                              | otherwise = sorted:split (take 2 unsorted) (drop 2 unsorted)
                                        where b = last sorted
                                              a = last (init sorted)

-- this function uses the split function in order to split the list into sorted sublists
splitSort :: Ord a => [a] -> [[a]]
splitSort [] = []
splitSort [a] = [[a]]
splitSort x = split (take 2 x) (drop 2 x)

-- Exercise 2
-- longest common sub-list of a finite list of finite list

--this function keeps in the list only the elements with the maximum length
keepMaxLen :: [[a]] -> [[a]]
keepMaxLen [] = []
keepMaxLen [a] = [a]
keepMaxLen a = [x | x<- a, (length x) == (maximum (map length a))]

-- this function adds all of the elements from the first list to the elements from the second, creating all the possible combinations.
addAllInt :: [Int] -> [Int]-> [[Int]]
addAllInt [] x = []
addAllInt (p:ps) xs = (p:xs):addAllInt ps xs

-- this function uses the previous function to add the elements from the first list to all of the lists given as a second parameter, creating all possible combinations
addToAllInt :: [Int] -> [[Int]] -> [[Int]]
addToAllInt [] x = x
addToAllInt ps [] = addAllInt ps []
addToAllInt ps (x:xs) = keepMaxLen(addAllInt ps x ++ addToAllInt ps xs)

-- this function returns the positions of an element in a list
pos :: Eq a => a -> [a] -> [Int]
pos x xs = [ index | (y,index) <- zip xs [0..], x==y ]

-- this function creates an index for a list
index :: Eq a => [a] -> [(a,Int)]
index a = zip a [0..]

-- this function returns the element from a given index
getPos :: Eq a => Int -> [(a,Int)] -> a
getPos x (a:ax) | x == snd a = fst a
                | otherwise = getPos x ax

-- this function returns all the possible combinations of positions of the common elements from the first list in the second
posList :: Eq a => [a] -> [a] -> [[Int]]
posList [] x = []
posList (x:xs) y = addToAllInt (pos x y) (posList xs y)

-- this function returns a sublist given the list of positions and the coresponding list
posToList :: Eq a => [Int] -> [a] -> [a]
posToList [] _ = []
posToList (x:xs) a = getPos x (index a) : posToList xs a

-- this function sorts the list ascending
ascSort :: [Int] -> [Int]
ascSort [] = []
ascSort (x:xs) = ascSort [y | y <- xs, y < x] ++ [x] ++ ascSort [y | y <- xs, y > x]

-- this function extractis the positions of the common sublists between two lists
subls :: Eq a => [a] -> [a] -> [[Int]]
subls [] x = []
subls x y = map ascSort (posList x y)

-- this function returns the list of sublists between two lists
subList :: Eq a => [[Int]] -> [a] -> [[a]]
subList [] a = []
subList (x:xs) a = posToList x a : subList xs a

-- this function counts the number of occurencies of an element in a list
nr :: Eq a => a -> [a] -> Int
nr x [] = 0
nr x (a:ax)   | (x==a) = 1 + nr x ax
              | otherwise = nr x ax

-- this function checks whether an element is existent in all lists of a list
isIn :: Eq a => a -> [[a]] -> Bool
isIn a [] = True
isIn a (x:xs) = elem a x && isIn a xs

-- this function creates the set of common elements with the incorrect number of repetitions
set :: Eq a => [a] -> [[a]] -> [a]
set [] x = []
set x [] = x
set (x:xs) a    | isIn x a = x : set xs a
                | otherwise = set xs a

 -- this function eliminates duplicates
noDuplicate :: Eq a => [a] -> [a]
noDuplicate [] = []
noDuplicate (x:xs)  | elem x xs = noDuplicate xs
                    | otherwise = x: noDuplicate xs

-- this function creates the set of common elements with the correct number of repetitions
subset :: Eq a => [a] -> [[a]] -> [a]
subset [] x = []
subset x [] = x
subset (x:xs) a = take n (repeat x) ++ subset xs a
                  where n = minimum [ nr x k | k <- a]

-- this function creates all common sublists between the lists from a list
-- this function should be called allSubLists (subset (noDuplicate (set (head x) (tail x))) x) x in order to return all the common sublists in x
allSubLists :: Eq a => [a] -> [[a]] -> [[[a]]]
allSubLists [] x = []
allSubLists x [] = []
allSubLists x (a:ax) = noDuplicate(keepMaxLen (subList (subls x a) a)) : allSubLists x ax

-- this function returns the intersection between the elements inside two lists
intersection :: Eq a => [[a]] -> [[a]] -> [[a]]
intersection x y = [ posToList (head (keepMaxLen (map asc (rmDup(posList a b))))) b | a<-x, b<-y]

-- this function returns the sublist of a list with ascending elements
asc :: [Int] -> [Int]
asc [] = []
asc [x] = [x]
asc (x:y:xs)  | (x<y) = x : asc (y:xs)
              | (x>y) = asc (x:xs)

-- this function checks whether a list contains duplicates or not
isDuplicate :: [Int] -> Bool
isDuplicate [] = False
isDuplicate (x:xs) = elem x xs || isDuplicate xs

-- this function removes duplicate lists
rmDup :: [[Int]] -> [[Int]]
rmDup [] = []
rmDup (x:xs)  | isDuplicate x = rmDup xs
              | otherwise = x: rmDup xs

-- this function returns all the common sublists in a list of lists
-- this function should be called commonSubLists (head k) (tail k) in order to return the common sublists within k
commonSubLists :: Eq a => [[a]] -> [[[a]]] -> [[a]]
commonSubLists [] x = []
commonSubLists x [] = []
commonSubLists x (a:ax)   | length ax == 0 = intersection x a
                          | otherwise = commonSubLists (intersection x a) ax

-- this function returns the longes common sublist within a list of lists
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList [x] = x
longestCommonSubList x  | length k == 0 = []
                        | otherwise = head (keepMaxLen (commonSubLists (head k) (tail k)))
                          where k = allSubLists (subset (noDuplicate (set (head x) (tail x))) x) x


-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

-- this function returns the sum of the credits from the passed modules
results :: [ModuleResult] -> Float
results [] = 0
results xs = sum [ credit x | x <- xs, mark x>=40]

-- this function checks whether the student is eligible for compensation
compensation :: [ModuleResult] -> Bool
compensation [] = True
compensation (x:xs) = (mark x >= 25) && compensation xs

-- this function returns whether the student has passed or not if the total achieved credits are more than or qeual to 60
canProgress :: [ModuleResult] -> Bool
canProgress xs  = (compensation xs) && ((results xs + 15) >= 60)

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)

-- this function returns the sum of marks with their weightings
marks :: [ModuleResult] -> Float
marks [] = 0
marks (x:xs) = credit x * fromIntegral(mark x) + marks xs

-- this function returns the sum of credits
credits :: [ModuleResult] -> Float
credits [] = 0
credits (x:xs) = credit x + credits xs

-- this function checks whether the student qualifies for a boost to get firts
qualifiesFirst :: [[ModuleResult]] -> Bool
qualifiesFirst xs = (result xs >= 68) && ((s2+t3*2+f4*2) / (sc+tc*2+fc*2) >= 0.5)
                    where s2 = credits (filter (\a -> mark a >=70) a)
                                  where a = head(tail xs)
                          sc = credits a
                                  where a = head(tail xs)
                          t3 = credits (filter (\b -> mark b >=70) b)
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          tc = credits b
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          f4 = credits (filter (\c -> mark c >=70) c)
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs
                          fc = credits c
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs

-- this function checks whether the student qualifies for a boost to get upper second
qualifiesUpperSecond :: [[ModuleResult]] -> Bool
qualifiesUpperSecond xs = (result xs >= 58) && ((s2+t3*2+f4*2) / (sc+tc*2+fc*2) >= 0.5)
                    where s2 = credits (filter (\a -> mark a >=60) a)
                                  where a = head(tail xs)
                          sc = credits a
                                  where a = head(tail xs)
                          t3 = credits (filter (\b -> mark b >=60) b)
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          tc = credits b
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          f4 = credits (filter (\c -> mark c >=60) c)
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs
                          fc = credits c
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs

-- this function checks whether the student qualifies for a boost to get lower second
qualifiesLowerSecond :: [[ModuleResult]] -> Bool
qualifiesLowerSecond xs = (result xs >= 48) && ((s2+t3*2+f4*2) / (sc+tc*2+fc*2) >= 0.5)
                    where s2 = credits (filter (\a -> mark a >=50) a)
                                  where a = head(tail xs)
                          sc = credits a
                                  where a = head(tail xs)
                          t3 = credits (filter (\b -> mark b >=50) b)
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          tc = credits b
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          f4 = credits (filter (\c -> mark c >=50) c)
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs
                          fc = credits c
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs

-- this function checks whether the student qualifies for a boost to get third
qualifiesThird :: [[ModuleResult]] -> Bool
qualifiesThird xs = (result xs >= 38) && ((s2+t3*2+f4*2) / (sc+tc*2+fc*2) >= 0.5)
                    where s2 = credits (filter (\a -> mark a >=40) a)
                                  where a = head(tail xs)
                          sc = credits a
                                  where a = head(tail xs)
                          t3 = credits (filter (\b -> mark b >=40) b)
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          tc = credits b
                                  where b | length xs == 3 = last xs
                                          | length xs == 4 = last (init xs)
                          f4 = credits (filter (\c -> mark c >=40) c)
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs
                          fc = credits c
                                  where c | length xs == 3 = []
                                          | length xs == 4 = last xs

-- this function calculates the average mark over the three or four years
result :: [[ModuleResult]] -> Float
result x    |length x == 3 = (0 * marks first + 1 * marks second + 2 * marks third)/(credits second + 2 * credits third)
            |length x == 4 = (0 * marks first + 1 * marks second + 2 * marks third + 2 * marks fourth)/(credits second + 2 * credits third + 2 * credits fourth)
                    where first = head x
                          second = head (tail x)
                          third | length x == 3 = last x
                                | length x == 4 = last (init x)
                          fourth = last x

-- this function returns the degre classification
classify :: [[ModuleResult]] -> DegreeClass
classify x  | (result x >= 70) || qualifiesFirst x = First
            | (result x >= 60) || qualifiesUpperSecond x = UpperSecond
            | (result x >= 50) || qualifiesLowerSecond x = LowerSecond
            | (result x <50) || qualifiesThird x = Third

-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s

-- this function returns an approximation of the maximum, using the golden search section algorithm
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x y eps | x>y = hillClimb d y x eps
                    | y-x <= (sqrt eps) = (x+y)/2
                    | d (x+invphi2*(y-x)) > d (x+invphi*(y-x)) = hillClimb d x (x+invphi*(y-x)) eps
                    | otherwise = hillClimb d (x+invphi2*(y-x)) y eps
                                where invphi = (sqrt(5.0)-1)/2.0
                                      invphi2 = (3-sqrt(5.0))/2.0

-- Exercise 6

-- this function creates a list of tuples containing the coefficients to the corresponding powers
coefficient :: [Float] -> [(Float,Float)]
coefficient x = zip x [0..]

-- this function creates the polynomial function from a list of coefficients
function :: [(Float,Float)] -> (Float -> Float)
function [] = \y -> 0*y
function (x:xs) = \y -> ((fst x)*y**(snd x)) + function xs y

-- this function calculates the nearest root by computing the minumum of the squared function
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x y eps  | x>y = nearestRoot xs y x eps
                        | y-x <= eps = (x+y)/2
                        | (f (x+invphi2*(y-x)))^2 < (f (x+invphi*(y-x)))^2 = nearestRoot xs x (x+invphi*(y-x)) eps
                        | otherwise = nearestRoot xs (x+invphi2*(y-x)) y eps
                                where invphi = (sqrt(5.0)-1)/2.0
                                      invphi2 = (3-sqrt(5.0))/2.0
                                      f = function (coefficient xs)

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)

-- this function adds the first two elements in the stack
addStack :: [Int] -> [Int]
addStack [] = []
addStack [a] = [a]
addStack (x:y:xs) = (x+y):xs

-- this function multiplies the first two elements in the stack
multiplyStack :: [Int] -> [Int]
multiplyStack [] = []
multiplyStack [a] = [a]
multiplyStack (x:y:xs) = (x*y):xs

-- this function duplicates the first element in the stack
duplicateStack :: [Int] -> [Int]
duplicateStack [] = []
duplicateStack (x:xs) = x:x:xs

-- this function pops the first element fromIntegral the stack
popStack :: [Int] -> [Int]
popStack [] = []
popStack (x:xs) = xs

-- this function executes the given lists of instruction on the given list of integers
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence [] ins = []
executeInstructionSequence stack [] = stack
executeInstructionSequence stack instructions@(i:ins)
          | i == Add = executeInstructionSequence (addStack stack) ins
          | i == Multiply = executeInstructionSequence (multiplyStack stack) ins
          | i == Duplicate = executeInstructionSequence (duplicateStack stack) ins
          | i == Pop = executeInstructionSequence (popStack stack) ins


-- Exercise 8

-- this function finds the optimal sequence of multiplication an duplication to get to the given power
optimalSequence :: Int -> [Instruction]
optimalSequence 0 = []
optimalSequence 1 = []
optimalSequence 2 = [Duplicate, Multiply]
optimalSequence n | even n = Duplicate : Multiply : optimalSequence (n `div` 2)
                  | odd n = (Duplicate : optimalSequence (n-1)) ++ [Multiply]

-- Exercise 9

-- this function adds all of the elements from the first list to the elements from the second, creating all the possible combinations.
addAll :: [Instruction] -> [Instruction] -> [[Instruction]]
addAll [] x = []
addAll (i:is) xs = (i:xs):addAll is xs

-- this function uses the previous function to add the elements from the first list to all of the lists given as a second parameter, creating all possible combinations
addToAll :: [Instruction] -> [[Instruction]]-> [[Instruction]]
addToAll [] x = x
addToAll is [] = addAll is []
addToAll is (x:xs) = keepMaxLen(addAll is x ++ addToAll is xs)

-- this function creates the list that contains all of the possible combinations of instructions
findAllBeavers :: [Int] -> [[Instruction]]
findAllBeavers [] = []
findAllBeavers [a] = []
findAllBeavers (x:xs) = addToAll [Pop,Multiply,Add] (findAllBeavers xs)

-- this function removes the lists of instructions that do not lead to the maximum result
removeUnwanted :: [Int] -> [[Instruction]] -> [[Instruction]]
removeUnwanted x ins = [ z | z <- ins, executeInstructionSequence x z == maximum (map (executeInstructionSequence x) ins) ]

-- this function resurns the list that contains all lists of instructions that lead to the maximum result
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers x = removeUnwanted x (findAllBeavers x++[[]])

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)

-- this function returns the x coordinate of the bottom left corner
ax :: Rectangle -> Int
ax (Rectangle (a,b) (c,d)) = a

-- this function returns the y coordinate of the bottom left corner
ay :: Rectangle -> Int
ay (Rectangle (a,b) (c,d)) = b

-- this function returns the x coordinate of the top right corner
bx :: Rectangle -> Int
bx (Rectangle (a,b) (c,d)) = c

-- this function returns the y coordinate of the top right corner
by :: Rectangle -> Int
by (Rectangle (a,b) (c,d)) = d

-- this function returns the list of rectangles that get in contact with the bottom left corner of the rectangle
bl :: Rectangle -> [Rectangle] -> [Rectangle]
bl r [] = []
bl rect (r:rectangles)  | (ax r <= ax rect) && (ay r <= ay rect) && (bx r >= ax rect) && (by r >= ay rect) = r: bl rect rectangles
                        | otherwise = bl rect rectangles

-- this function returns the list of rectangles that get in contact with the top left corner of the rectangle
tl :: Rectangle -> [Rectangle] -> [Rectangle]
tl r [] = []
tl rect (r:rectangles)  | (ax r <= ax rect) && (by r >= by rect)&& (bx r >= ax rect) && (ay r <= by rect) = r: tl rect rectangles
                        | otherwise = tl rect rectangles

-- this function returns the list of rectangles that get in contact with the bottom right corner of the rectangle
br :: Rectangle -> [Rectangle] -> [Rectangle]
br r [] = []
br rect (r:rectangles)  | (bx r >= bx rect) && (ay r <= ay rect)&& (ax r <= bx rect) && (by r >= ay rect) = r: br rect rectangles
                        | otherwise = br rect rectangles

-- this function returns the list of rectangles that get in contact with the top right corner of the rectangle
tr :: Rectangle -> [Rectangle] -> [Rectangle]
tr r [] = []
tr rect (r:rectangles)  | (bx r >= bx rect) && (by r >= by rect)&& (ax r <= bx rect) && (ay r <= by rect) = r: tr rect rectangles
                        | otherwise = tr rect rectangles

-- this function removes a rectangle from the list
remove :: Rectangle -> [Rectangle] -> [Rectangle]
remove r [] = []
remove rect (r:rectangles)  | rect == r = remove r rectangles
                            | otherwise = r: remove rect rectangles

-- this function returns the list of rectangle without duplicates
union :: [Rectangle] -> [Rectangle]
union [] = []
union (r:rectangles)  | elem r rectangles = union rectangles
                      | otherwise = r : union rectangles

-- this function returns the list of rectangles that a rectangle gets in contact with
neighbourhood :: Rectangle -> [Rectangle] -> [Rectangle]
neighbourhood r rct = r : union ((bl r rectangles ++ tl r rectangles ++ br r rectangles ++ tr r rectangles))
                      where rectangles = remove r rct

-- this function returns part of the image that it's isolated
img :: [(Rectangle,Bool)] -> [Rectangle] -> [Rectangle]
img r [] = []
img r (x:xs)  | [snd k | k <- r , fst k == x] == [False] = (neighbourhood x (map fst r)) ++ img (visited x r) xs ++ img (visited x r) (neighbourhood x (map fst r))
              | otherwise = img r xs

-- this function returns the list of lists of rectangles that a list of rectangles gets in contact with
allNeighbourhoods :: [Rectangle] -> [Rectangle] -> [[Rectangle]]
allNeighbourhoods r [] = []
allNeighbourhoods r (x:xs) = img f (neighbourhood x r) : allNeighbourhoods r xs
                      where f=map (\x -> (x,False)) r

-- this function says that a rectangle has been already visited
visited :: Rectangle -> [(Rectangle,Bool)] -> [(Rectangle,Bool)]
visited r [] = []
visited r (x:xs)  | r == fst x = (r,True) : visited r xs
                  | otherwise = x : visited r xs

-- this function checks wheter the elements in the first list cand be found in the second list
equivalent :: [Rectangle] -> [Rectangle] -> Bool
equivalent [] y = True
equivalent (x:xs) y = elem x y && equivalent xs y

-- this function checks whether a set of rectangles is equivalent to one of the rectangles list in the list of lists given as the second parameter
isEquivalent :: [Rectangle] -> [[Rectangle]] -> Bool
isEquivalent r [] = False
isEquivalent r (x:xs) = (equivalent x r && equivalent r x && length x == length r) || isEquivalent r xs

-- this function removes the equivalent lists
rmEquivalents :: [[Rectangle]] -> [[Rectangle]]
rmEquivalents [] = []
rmEquivalents (x:xs)    | isEquivalent x xs = rmEquivalents xs
                        | otherwise = x : rmEquivalents xs
-- this function returns a list contain all lists of rectangles that form a group
allImages :: [Rectangle] -> [[Rectangle]]
allImages [] = []
allImages r = rmEquivalents (map noDuplicate (allNeighbourhoods r r))

-- this function returns the points a list of rectangles covers
pts :: [Rectangle] -> [(Int,Int)]
pts [] = []
pts (r:rs) = [(x,y) | x <- [ax r .. bx r], y <- [ay r .. by r]] ++ pts rs

-- this function returns the list of lists representing the points in each group of rectangles
areas :: [[Rectangle]] -> [[(Int,Int)]]
areas [] = []
areas (r:rs) = sort(noDuplicate (pts r)) : areas rs

-- this function sorts the points ascending by the x coordinate
fstSort :: [(Int,Int)] -> [(Int,Int)]
fstSort [] = []
fstSort (x:xs) = fstSort [y | y <- xs, fst y <= fst x] ++ [x] ++ fstSort [y | y <- xs, fst y > fst x]

-- this function groups the sets of points by the second coordinate
group :: [(Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
group [] [] = []
group x [] = [x]
group k (x:xs)  | fst (last k) == fst x =  group (k++[x]) xs
                | otherwise = k: group [x] xs

-- this function sorts the points by the y coordinate
sndSort :: [(Int,Int)] -> [(Int,Int)]
sndSort [] = []
sndSort (x:xs) = sndSort [y | y <- xs, snd y < snd x] ++ [x] ++ sndSort [y | y <- xs, snd y > snd x]

-- this function sorts the points ascending by the x coordinate with respect to the y coordinate
sort :: [(Int,Int)] -> [(Int,Int)]
sort [] = []
sort x = foldr (++) [] (map sndSort (group [head (fstSort x)] (tail (fstSort x))))

-- this function finds all bottol left corners
findBLvertices :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findBLvertices [] r = []
findBLvertices (x:xs) r  | (not (elem (a-1,b) r)) && (not (elem (a,b-1) r)) = x : findBLvertices xs r
                      | otherwise = findBLvertices xs r
                          where a = fst x
                                b = snd x

-- this function finds all top left corners
findTLvertices :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findTLvertices [] r = []
findTLvertices (x:xs) r  | (not (elem (a-1,b) r)) && (not (elem (a,b+1) r)) = x : findTLvertices xs r
                      | otherwise = findTLvertices xs r
                          where a = fst x
                                b = snd x

-- this function finds all bottol right corners
findBRvertices :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findBRvertices [] r = []
findBRvertices (x:xs) r  | (not (elem (a,b-1) r)) && (not (elem (a+1,b) r)) = x : findBRvertices xs r
                      | otherwise = findBRvertices xs r
                          where a = fst x
                                b = snd x

-- this function finds all top right corners
findTRvertices :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findTRvertices [] r = []
findTRvertices (x:xs) r  | (not (elem (a+1,b) r)) && (not (elem (a,b+1) r)) = x : findTRvertices xs r
                      | otherwise = findTRvertices xs r
                          where a = fst x
                                b = snd x

--this function builds the maximum rectangle from the bottom left corner
buildBL :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Rectangle
buildBL s f [] = Rectangle s s
buildBL s f r   | (elem (c + 1, d + 1) r) && (elem (a, d + 1) r) && (elem (c +1, b) r) = buildBL s (c+1,d+1) r
                | (elem (c + 1, d) r) && (elem (c+1, b) r) = buildBL s (c + 1, d) r
                | (elem (c, d + 1) r) && (elem (a, d+1) r) = buildBL s (c, d + 1) r
                | otherwise =  Rectangle s f
                      where a = fst s
                            b = snd s
                            c = fst f
                            d = snd f

--this function builds the maximum rectangle from the top left corner
buildTL :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Rectangle
buildTL s f [] = Rectangle s s
buildTL s f r   | (elem (c + 1, d - 1) r) && (elem (a, d - 1) r) && (elem (c + 1, b) r) = buildTL s (c + 1,d - 1) r
                | (elem (c + 1, d) r) && (elem (c + 1, b) r) = buildTL s (c + 1, d) r
                | (elem (c, d - 1) r) && (elem (a, d - 1) r) = buildTL s (c, d - 1) r
                | otherwise =  Rectangle (a,d) (c,b)
                      where a = fst s
                            b = snd s
                            c = fst f
                            d = snd f

--this function builds the maximum rectangle from the bottom right corner
buildBR :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Rectangle
buildBR s f [] = Rectangle s s
buildBR s f r   | (elem (c - 1, d + 1) r) && (elem (a, d + 1) r) && (elem (c - 1, b) r) = buildBR s (c - 1,d + 1) r
                | (elem (c - 1, d) r) && (elem (c - 1, b) r) = buildBR s (c - 1, d) r
                | (elem (c, d + 1) r) && (elem (a, d + 1) r) = buildBR s (c, d + 1) r
                | otherwise =  Rectangle (c,b) (a,d)
                      where a = fst s
                            b = snd s
                            c = fst f
                            d = snd f

--this function builds the maximum rectangle from the top right corner
buildTR :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Rectangle
buildTR s f [] = Rectangle s s
buildTR s f r   | (elem (c - 1, d - 1) r) && (elem (a, d - 1) r) && (elem (c - 1, b) r) = buildTR s (c - 1,d - 1) r
                | (elem (c - 1, d) r) && (elem (c - 1, b) r) = buildTR s (c - 1, d) r
                | (elem (c, d - 1) r) && (elem (a, d - 1) r) = buildTR s (c, d - 1) r
                | otherwise =  Rectangle f s
                      where a = fst s
                            b = snd s
                            c = fst f
                            d = snd f

-- this function removes the rectangles that are not valid
rectangles :: [Rectangle] -> [Rectangle]
rectangles r = [ x | x <- r, (ax x <= bx x) && (ay x <= by x)]

-- this funtion creates a list containing lists on the following pattern: cl vertices, br vertices, tl vertices, tr vertices for each group of rectangles
findVertices :: [[(Int,Int)]] -> [[[(Int,Int)]]]
findVertices [] = []
findVertices (r:rs) = (findBLvertices r r : findBRvertices r r : findTLvertices r r : findTRvertices r r : []) : findVertices rs

-- this function builds rectangles, given the state of the vertice (bl,tr,br,tl)
b :: ((Int,Int) -> (Int,Int) -> [(Int,Int)] -> Rectangle) -> [(Int,Int)] -> [(Int,Int)] -> [Rectangle]
b build [] r = []
b build (x:xs) r = build x x r : b build xs r

-- this function builds the unsimplified image
build :: [([(Int,Int)],[[(Int,Int)]])] -> [Rectangle]
build [] = []
build (r:rs) = b buildBL (head(snd r)) (fst r) ++ b buildBR (head (tail (snd r))) (fst r) ++ b buildTL (last (init (snd r))) (fst r) ++ b buildTR (last (snd r)) (fst r) ++ build rs

-- this function builds the simplified image of the list of rectangles
-- this function should be called simplify x x in order to simplify the list x
simplify :: [Rectangle] -> [Rectangle] -> [Rectangle]
simplify [] r = []
simplify (x:xs) r   | areas (allImages (remove x r)) == areas (allImages r) = simplify xs (remove x r)
                    | otherwise = x : simplify xs r

-- this function returns the shortest simplified list of rectangles
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList r = simplify (noDuplicate (build (k))) (noDuplicate (build (k)))
                            where k = zip (areas (allImages (rectangles r))) (findVertices (areas (allImages (rectangles r))))


-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image

-- simplifyEllipse :: [Rectangle] -> [Rectangle] -> [Rectangle]
-- simplifyEllipse [] r = []
-- simplifyEllipse (x:xs) r  | sort (noDuplicate (pts (remove x r))) == sort (noDuplicate (pts r)) = simplifyEllipse xs (remove x r)
--                           | otherwise = x : simplifyEllipse xs r


-- this function builds all rectangles existent within the area of the ellipse
-- it uses simplifyRectangleList function from the previous exercise
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b  =  simplifyRectangleList r
                              where r = rectangles [(Rectangle ( k, l) ( m, n)) | k <- z, l <- w, m <- z, n <- w, ((fromIntegral(k)-x)^2/a^2 + (fromIntegral(l)-y)^2/b^2) <= 1, ((fromIntegral(m)-x)^2/a^2 + (fromIntegral(n)-y)^2/b^2) <= 1, ((fromIntegral(k)-x)^2/a^2 + (fromIntegral(n)-y)^2/b^2) <= 1, ((fromIntegral(m)-x)^2/a^2 + (fromIntegral(l)-y)^2/b^2) <= 1]
                                            where z = [( ceiling (x-a))..(floor (x+a))]
                                                  w = [( ceiling (y-b))..(floor (y+b))]

-- Exercise 12
-- extract a message hidden using a simple steganography technique

-- this function extracts the 0's and 1's from the String
extract :: String -> String
extract [] = []
extract (x:xs)  | (x=='0') || (x=='1') = x : extract xs
                | otherwise = extract xs

-- this function decodes the string of 0's and 1's
decode :: String -> String
decode [] = []
decode [x] = []
decode (x:y:xs) = case [x,y] of   "00" -> "a" ++ decode xs
                                  "01" -> "b" ++ decode xs
                                  "10" -> "c" ++ decode xs
                                  "11" -> "d" ++ decode xs

-- this function decodes the message
extractMessage :: String -> String
extractMessage s = decode (extract s)

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method

-- this method follows the diagonalisation algorithm in order to return a different stream
-- this method should be called stream [] x in order to return a different stream which is different from all streams in x
stream :: [[Int]] -> [[Int]] -> [Int]
stream [] [] = []
stream x [] = heads x ++ stream (tails x) []
          where heads l = [ head a + 1 | a <- l]
                tails l = [ tail a | a <- l]
stream x (y:ys) = heads (xs) ++ stream (tails xs) ys
                where xs = y: x
                      heads l = [ head a + 1 | a <- l]
                      tails l = [ tail a | a <- l]

-- this method returns a stream which is different from all streams of the given stream
differentStream:: [[Int]] -> [Int]
differentStream x = stream [] x

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function

-- this function unpaires the given number
find :: Int -> (Int,Int)
find n  | (n-m^2) < m = (n-m^2,m)
        | otherwise = (m, m^2+2*m-n)
                where m = floor (sqrt (fromIntegral n))

-- this function applies the given function to the pair coresponding to the given number
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f (fst (find n)) (snd (find n))

-- Exercise 15
data Tree = Empty | Node Int Tree Tree deriving Show

-- this function builds the corresponding tree starting with the root n
buildTree :: Int -> Tree
buildTree 0 = (Node 0 Empty Empty)
buildTree 1 = (Node 1 Empty (Node 0 Empty Empty))
buildTree n = (Node n (buildTree (fst (find n))) ( buildTree (snd (find n))))

-- this function sums all nodes in the tree
treeSum :: Tree -> Int
treeSum Empty = 0
treeSum (Node n x y) = n + treeSum x + treeSum y

-- this function verifies if the sum of the left side o the tree formed out of the given number is equal to the right immediate node
isShellTreeSum :: Int -> Bool
isShellTreeSum n = (treeSum (buildTree (fst (find n))) - (fst (find n))) == (snd (find n))
