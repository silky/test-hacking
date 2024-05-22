module Main where


import Prelude hiding (init)
import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck


data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)


instance Arbitrary State where
  arbitrary = elements [ Open, Closed, Final ]


{-
   TODO:

- [ ] Model our state!
- [ ] Think about transitions
- [ ] Explore design choices
- [ ] Make sure we can represent what we want! Step through states?
- [ ] Maybe some tests
    - [x] Can't get to invalid states?
    - [ ] Can get to some expected states?

Things to think about:
  - [ ] Save increments and decrements
-}

init :: State
init = Open

close :: State -> Maybe State
close = \case
  Open   -> Just Closed
  Closed -> Just Closed
  _      -> Nothing -- Here

finalise :: State -> Maybe State
finalise = \case
  Closed -> Just Final
  Final  -> Just Final
  _      -> Nothing


{-
Properties!
  - [ ] Make sure all transitions move right
  - [ ] If we do enough (non-inc/dec) transitions we get to final; i.e. it terminates.
  - [ ] Can always close and finalise any state.

  Stretch goal:
  - [ ] Finite # of decrements (given no increments being applied)
-}


prop_canAlwaysCloseAndFinaliseAnyState :: State -> Property
prop_canAlwaysCloseAndFinaliseAnyState state =
  (close state >>= finalise) === Just Final .||. finalise state === Just Final

{-

   Hmm.
    State -> Bool

  Ways to close a state
    -> Open -> Closed
    -> Closed -> Itself -- Illegal
    -> Final -> ?? -- Illegal

  Ways to finalise
    -> Closed -> Finalise
    -> Illegal



-}


main :: IO ()
main = do
  hspec $ do
    it "can't go from init to finalise" $ do
      finalise init `shouldBe` Nothing
    it "can go to final from closed" $ do
      (close init >>= finalise) `shouldBe` Just Final

    describe "properties" $ do
      it "Can always close and finalise any state"
        $ property prop_canAlwaysCloseAndFinaliseAnyState
