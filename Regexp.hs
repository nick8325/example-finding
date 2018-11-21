{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE NoRecursiveDo, TypeApplications #-}

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import QuickSpec hiding (E)
import BadExamples

data TestCase = TestCase { regexp :: E It, input :: It }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary TestCase where
  arbitrary = liftM2 TestCase arbitrary arbitrary
  shrink = genericShrink

tests :: [TestCase]
tests = [
  --TestCase {regexp = Nil, input = []},
  --TestCase {regexp = Nil :+ Eps, input = []},
  --TestCase {regexp = Eps, input = [a]},
  --TestCase {regexp = Atom (a), input = [a]},
  --TestCase {regexp = Atom (b), input = []},
  --TestCase {regexp = Star (Atom (b)), input = [b]},
  --TestCase {regexp = Nil, input = [a]}]
  TestCase {regexp = Nil, input = b}]
  --TestCase {regexp = Nil :+ Eps, input = a},
  --TestCase {regexp = Eps, input = a},
  --TestCase {regexp = Atom (a), input = a},
  --TestCase {regexp = Atom (b), input = b},
  --TestCase {regexp = Star (Atom (b)), input = b},
  --TestCase {regexp = Nil, input = a}]
--  TestCase Nil []]
--  TestCase Eps [],
--  TestCase (Star (Atom a)) [a,a,a],
--  TestCase (Atom a :> Atom b) [a,b],
--  TestCase (Atom a :+ Atom b) [a],
--  TestCase (Atom a :+ Atom b) [b],
--  TestCase (Atom a :> Atom b) [b,a] ]
  where
    a = A
    b = B

data E a
  = E a :> E a
  | E a :+ E a
  | Atom a
  | Star (E a)
  | Eps
  | Nil
 deriving ( Eq, Ord, Show )

-- ASK: any of these right-hand sides

(.>), (.+) :: E a -> E a -> E a
Nil .> q   = Nil
p   .> Nil = Nil
Eps .> q   = q
p   .> Eps = p
p   .> q   = p :> q

Nil .+ q   = q
p   .+ Nil = p
Eps .+ Eps = Eps
p   .+ q   = p :+ q

star :: E a -> E a
star Eps = Eps
star Nil = Eps
star p   = Star p

eps :: E a -> E a
eps Nil      = Nil
eps (Atom _) = Nil
eps Eps      = Eps
eps (p :+ q) = eps p .+ eps q
eps (p :> q) = eps p :> eps q
eps (Star _) = Eps

step :: Eq a => E a -> a -> E a
step (p :> q) a             = (step p a .> q) .+ (eps p .> step q a)
step (p :+ q) a             = step p a .+ step q a
step (Atom c) a | a == c    = Eps
                | otherwise = Nil
step (Star p) a             = step p a .> Star p
step _        _             = Nil

steps :: Eq a => E a -> [a] -> E a
steps p []     = p
steps p (a:as) = steps (step p a) as

rec :: Eq a => E a -> [a] -> Bool
rec p as = eps (steps p as) == Eps

--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (E a) where
  arbitrary = sized arb
   where
    arb n = frequency
      [ (1, return Nil)
      , (1, return Eps)
      , (1, Atom `fmap` arbitrary)
      , (n, Star `fmap` arb2)
      , (n, liftM2 (:>) arb2 arb2)
      , (n, liftM2 (:+) arb2 arb2)
      ]
     where
      arb2 = arb (n `div` 2)

  shrink (Atom a) = [ Nil, Eps ] ++ [ Atom a' | a' <- shrink a ]
  shrink Eps      = [ Nil ]
  shrink (Star p) = [ p, p :> p ] ++ [ Star p' | p' <- shrink p ]
  shrink (p :> q) = [ p, q ] ++ [ p' :> q | p' <- shrink p ]
                             ++ [ p :> q' | q' <- shrink q ]
  shrink (p :+ q) = [ p, q ] ++ [ p' :+ q | p' <- shrink p ]
                             ++ [ p :+ q' | q' <- shrink q ]
  shrink _        = []

data It = A | B deriving (Eq, Ord, Show)
instance Arbitrary It where arbitrary = elements [A, B]

main =
  --quickSpec [ instFun (elements tests),
  examples tests [
    monoType (Proxy :: Proxy It),
    monoType (Proxy :: Proxy (E It)),
    monoTypeWithVars ["t"] (Proxy :: Proxy TestCase),
    bools,
    lists,
    withMaxTestSize 20,
    background [
      con "Atom" (Atom @It),
      con "Eps" (Eps @It),
      con "Nil" (Nil @It),
      con ".>" ((.>) @It),
      con ".+" ((.+) @It),
      con "star" (star @It),
      con "eps" (eps @It),
      --con "step" (step @It),
      con "regexp" regexp,
      con "input" input,
      con "rec" (rec @It),
    --  con "TestCase" TestCase,
      con "null" (null @[] @It)],
    con "teststep" (\(TestCase x y) -> step x y)] >>= mapM_ print
    --con "testrec" (\(TestCase x y) -> rec x y)]
