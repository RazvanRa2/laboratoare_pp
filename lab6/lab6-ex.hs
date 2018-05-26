{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import System.Random

{-
=========== Cum rulam testele! ======================
- verificare ex.1:
  check check1

- verificare toate ex.:
  checkAll
=====================================================
-}

{-
1. (1p)
Construiți funcții simple pentru următoarele tipuri (completați definițiile):
-}
identity :: a -> a
identity x = x

isZero :: Int -> Bool
isZero x = x == 0

succesor :: Int -> Int
succesor x =  x + 1

listsToPair :: [a] -> [b] -> ([a], [b])
listsToPair l1 l2 = (l1, l2)

-- Verificare: check1
check1 :: TestPP ()
check1 = do

  assertVal "[1] listsToPair [isZero 3] undefined" 0.25 $ -- 0.25p
    not $ head $ fst $ listsToPair [isZero 3] undefined
  assertVal "[1] identity [isZero 0]" 0.25 $ -- 0.25p
    head $ identity $ [isZero 0]
  assertVal "[1] listsToPair [isZero 0] [isZero (succesor 0)]" 0.25 $ -- 0.25p
    let res = listsToPair [isZero 0] [isZero (succesor 0)] in (head (fst res)) && (not (head (snd res)))
  assertVal "[1] listsToPair [1, 2, 3] undefined" 0.25 $ -- 0.25p
    fst (listsToPair [1..3] undefined) == [1..3]


{-
2. (1p)
Implementați funcția `unzip2`
-}
unzip2  :: (Eq a, Eq b) => [(a, b)] -> ([a], [b])
unzip2 x = (map fst x, map snd x)

-- Verificare: check2
check2 :: TestPP ()
check2 = do
  assertVal "[2] unzip2 (zip)" 1 $ -- 1p
    unzip2 (zip [1,2,3] ["a","b","c"]) == ([1,2,3], ["a","b","c"])


{-
3. (1p)
Implementați funcția `zipWith2`
Hint: Poate să fie utilă funcția `zip`
-}
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f l1 l2 = map (\x -> f (fst x) (snd x)) (zip l1 l2)

-- Verificare: check3
check3 :: TestPP ()
check3 = do
  assertVal "[3] zipWith2 (+)" 1 $ -- 1p
    zipWith2 (+) [1, 2, 3] [4, 5, 6, 7, 8] == [5,7,9]


{-
4. (1p)
Implementați funcția `insertAt` care adaugă într-o listă un element la o anumită poziție
-}
insertAt :: a -> Int -> [a] -> [a]
insertAt elem index list = ((take index list) ++ [elem]) ++ (drop index list)

-- Verificare: check4
check4 :: TestPP ()
check4 = do
  assertVal "[4] insertAt 'top' 2 ['This', 'is', 'secret']" 1 $ -- 1p
    insertAt "top" 2 ["This", "is", "secret"]  == ["This", "is", "top", "secret"]


{-
5. (1p)
Implementați funcția `generateInfList`, care pornește de la un o listă de cuvinte și formează
o listă infinită prin repetarea acestora.
Hint: Poate să fie utilă funcția `repeat`
-}
generateInfList :: [String] -> [String]
generateInfList seed = seed ++ (generateInfList seed)

-- Verificare: check5
check5 :: TestPP ()
check5 = do
  assertVal "[5] generateInfList ['very', 'long', 'sentence'] " 1 $ -- 1p
    (generateInfList ["very", "long", "sentence"]) !! 1234 == "long" &&
    (generateInfList ["very", "long", "sentence"]) !! 5678 == "sentence" &&
    (generateInfList ["very", "long", "sentence"]) !! 3333 == "very"


{-
6. (1p)
Implementați funcția `endList`, care trunchiază o listă de cuvinte eventual infinită la primul
cuvânt terminal, incluzându-l și pe acesta
Hint: Poate să fie utilă funcția `span`
-}
delimiters:: [String]
delimiters = ["\n", ".", "!", "?"]
endList :: [String] -> [String]
endList l = undefined

-- Verificare: check6
test_i :: Int
test_i = 1234

rand_i :: Int
rand_i = 9999

getRandElem :: [String] -> String
getRandElem l = l !! (fst $ randomR (0, length l - 1) g)
  where
    g = mkStdGen rand_i

infList :: [String]
infList = generateInfList ["very", "long", "sentence"]

check6 :: TestPP ()
check6 = do
  assertVal "[6] endList infList" 1 $ -- 1p
   length ( endList  (insertAt (getRandElem delimiters) test_i infList) ) == test_i + 1


{-
7. (3p)
Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
intersecție, diferență, produs cartezian. Utilizați ulterior funcțiile definite anterior
pentru a reprezenta reuniunea mulțimilor.
-}
setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection a b = [x | elem x a, elem x b]

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff a b = [x | x<-a, notElem x b]

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct a b = [(x,y) | x <-a, x<-b]

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion a b = (setIntersection a b) ++ (setDiff a b) ++ (setIntersection b a)

-- Verificare: check7
check7 :: TestPP ()
check7 = do
  assertVal "[7] cartProduct" 0.5 $ -- 0.5p
    cartProduct [1, 2] [3, 4, 5] == [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)]
  let a = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]
  assertVal "[7] setIntersection" 0.75 $ -- 0.75p
    sort (setIntersection a b) == [1, 2, 6]
  assertVal "[7] setDiff" 0.75 $ -- 0.75p
    sort (setDiff a b) == [3, 7]
  assertVal "[7] setUnion" 1 $ -- 1p
    sort (setUnion a b) == [1, 2, 3, 4, 6, 7, 8, 10]


{-
8. (1p)
Verificaţi dacă o propoziţie este palindrom. O propoziţie este palindrom dacă conţine cuvintele
în aceeaşi ordine, fie ea citită de la început spre sfârşit sau de la sfârşit spre început.
Exemplu: Ana este ingenioasa si ingenioasa este Ana -> True

Hint: s-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi `words`
pentru a obține cuvintele din frază și `unwords` sau `++` pentru a obține
frază din cuvinte.
-}

palindrome :: String -> Bool
palindrome s = let list = (words s)
			   in foldr (&&) True (zipWith2 (==) list (reverse list)) 

-- Verificare: check8
check8 :: TestPP ()
check8 = do
  let prop = "Ana este ingenioasa si ingenioasa este Ana" in
    assertVal "[8] palindrome" 0.5 $ -- 0.5p
      palindrome prop
  let prop2 = "Ana bea cafea si cafea nu bea Ana" in
    assertVal "[8] not palindrome" 0.5 $ -- 0.5p
      not $ palindrome prop2


{- BONUS
9. (2p)
Implementați o funcție ce calculează cmmdc-ul unei liste de numere pozitive.
Lista va avea lungime minima 2.
-}
cmmdc :: [Integer] -> Integer
cmmdc l = undefined

-- Verificare: check5
check9 :: TestPP ()
check9 = do
  assertVal "[9] cmmdc" 1 $ -- 1p
    cmmdc [6, 24, 18, 33, 99] == 3
  assertVal "[9] cmmdc x y" 1 $ -- 1p
    (\l -> let c = cmmdc l in and (map (\x -> x `mod` c == 0) l)) [4, 5, 8, 21]


{- BONUS
10. (1p)
Duplicaţi toate cuvintele dintr-o propoziţie.
Exemplu: Ce laborator frumos! -> Ce Ce laborator laborator frumos! frumos!
Hint: Ar putea fi utile funcţiile concat sau "++" pentru concatenarea cuvintelor.
-}
dup :: String -> String
dup sentence = undefined

-- Verificare: check10
check10 :: TestPP ()
check10 = do
  assertVal "[10] dup" 0.5 $ -- 0.5p
    dup "Ce laborator frumos!" == "Ce Ce laborator laborator frumos! frumos!"
  assertVal "[10] dup, again" 0.5 $ -- 0.5p
    null $ (\sentence -> filter (/= 2) $ map length $ group $ words $ dup sentence) "To be or not to be"


{- BONUS
11. (2p)
Găsiţi numărul de apariţii ale fiecărui element dintr-o listă în lista respectivă.
Rezultatul va fi returnat ca o listă de tupluri, în care primul element al perechii
va fi elementul din listă, iar al doilea element al perechii va fi numărul de apariţii în listă.
Cum rezultatul va fi similar unui dicţionar, chiar dacă un element va apărea de mai multe ori în listă,
va trebui să fie prezent într-o singură pereche în dicţionar.

Hint: s-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi `group` pentru
a grupa elementele egale în liste separate şi funcţia sort pentru a sorta o listă.
-}
nrOcc :: Ord a => [a] -> [(a, Int)]
nrOcc l = undefined

-- Verificare: check11
check11 :: TestPP ()
check11 =
  assertVal "[11] number of occurrences" 2 $ -- 2p
    nrOcc [1, 2, 3, 4, 2, 3, 3, 1, 2, 3, 3, 4] == [(1, 2), (2, 3), (3, 5), (4, 2)]

{-
Helpers for testing :)
-}
checkAll = check $
  sequence_[check1, check2, check3, check4, check5, check6, check7, check8, check9, check10, check11]
