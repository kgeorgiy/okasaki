module List

{- Exercise 2.1 -}
vsuffixes : Vect a n -> Vect (List a) (S n)
vsuffixes [] = [[]]
vsuffixes (x :: xs) = let ss = vsuffixes xs in (x :: head ss) :: ss

lsuffixes : List a -> List (List a)
lsuffixes [] = [[]]
lsuffixes (x :: xs) with (lsuffixes xs)
    | (s :: ss) = (x :: s) :: s :: ss

lsuffixes2 : List a -> (r : List (List a) ** isCons r = True)
lsuffixes2 [] = ([[]] ** ?p)
lsuffixes2 (x :: xs) with (lsuffixes2 xs)
    | (ss ** p) = ((x :: head ss p) :: ss ** ?p2)

lsuffixes3 : List a -> (r : List (List a) ** isCons r = True)
lsuffixes3 = foldr next ([[]] ** ?p3) where
    next : a -> (ss : List (List a) ** isCons ss = True) -> (r : List (List a) ** isCons r = True)
    next x (ss ** p) = ((x :: head ss p) :: ss ** ?p4)

checkSuffixes : (List Int -> List (List Int)) -> Bool
checkSuffixes f = Prelude.List.and [
    f [] == [[]],
    f [1, 2, 3] == [[1, 2, 3], [2, 3], [3], []]
] 

-- cs : (List a -> List (List a)) -> Bool
-- cs f = f [1, 2, 3] == [[1, 2, 3], [2, 3], [3], []]

testLSuffixes : so (checkSuffixes lsuffixes)
testLSuffixes = oh

testLSuffixes2 : so (checkSuffixes (getWitness . lsuffixes2))
testLSuffixes2 = oh

testLSuffixes3 : so (checkSuffixes (getWitness . lsuffixes3))
testLSuffixes3 = oh

testVSuffixes : so (checkSuffixes (\xs => toList $ vsuffixes $ fromList xs))
testVSuffixes = oh


---------- Proofs ----------
List.p2 = proof {
  intros;
  trivial;
}

List.p = proof {
  intro;
  trivial;
}


---------- Proofs ----------

List.p4 = proof {
  intro;
  intro;
  intro;
  intro;
  intro;
  compute;
  refine refl;
}

List.p3 = proof {
  intro;
  compute;
  refine refl;
}

