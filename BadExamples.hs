{-# LANGUAGE ScopedTypeVariables #-}
module BadExamples where

import QuickSpec as QS hiding (quickSpec)
import QuickSpec.Haskell hiding (con, TestCase)
import QuickSpec.Testing
import QuickSpec.Prop
import QuickSpec.Type hiding (In)
import QuickSpec.Utils
import qualified QuickSpec.Term as QuickSpec
import QuickSpec.Explore.PartialApplication(PartiallyApplied)
import qualified QuickSpec.Testing.QuickCheck as QuickCheck
import Twee.Base(Extended, Term)
import Test.QuickCheck
import Test.QuickCheck.Poly
import Data.Maybe
import Data.List
import Control.Monad
import Data.Functor.Identity

examples :: forall t sig. (Show t, Typeable t, Signature sig) => [t] -> sig -> IO [t]
examples tests sig = do
  thy <- quickSpec cfg
  loop thy
  where
    loop [] = return tests
    loop (prop:props) = do
      res <- testcases prop
      case res of
        Nothing -> loop props
        Just [] -> loop props
        Just tcs -> do
          mapM_ print tcs
          examples (tests ++ tcs) sig

    cfg =
      unSig (signature sig `mappend` hardcoded)
        (Context 1 []) defaultConfig
    cfg' =
      unSig (signature sig)
        (Context 1 []) defaultConfig
    insts = cfg_instances cfg' `mappend` baseInstances
    hardcoded = instFun (elements tests)

    testcases prop = generate $ do
      Identity mcex <- QuickCheck.run (cfg_quickCheck cfg') (arbitraryTestCase (cfg_default_to cfg') insts) eval (test prop)
      case mcex of
        Nothing -> return Nothing
        Just cex ->
          return $ Just $
            [ y
            | x <- usort (QuickSpec.vars prop),
              typ x == typeOf (undefined :: t),
              Just val <- [tc_eval_var cex x],
              Just (Identity y) <- [fromValue val] ]

    eval = evalHaskell (cfg_default_to cfg) insts
