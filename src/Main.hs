module Main where


import Prelude hiding (init)
import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck
import "base" Data.Function ((&))


data SomeState =
  SomeState
    { ints :: [Int]
    }
    deriving (Eq, Show)

emptyState :: SomeState
emptyState = SomeState []

-- Could be instead:
--
-- data State'
--   = State'
--     { state    :: State
--     , contents :: SomeState
--     }

data State
  = Open   SomeState
  | Closed SomeState
  | Final  SomeState
  deriving (Show, Eq)

instance Arbitrary SomeState where
  arbitrary = SomeState <$> arbitrary

instance Arbitrary State where
  arbitrary = oneof
                [ Open   <$> arbitrary
                , Closed <$> arbitrary
                , Final  <$> arbitrary
                ]

{- Thoughts around state transitions actually doing things!


newtype Transition = Transition { mkTransition :: ReadContext -> State -> Maybe State }

data Transition
  = DoOpen
  | DoClose
  | DoFinal

transitions :: [Transition]


apply :: ReadContext -> State -> [Transition] -> IO State


apply1 rc state t@DoOpen = do
  let tx = buildOpenTranscation state
  sendTransition tx
  pure $ transitionState t state



- Questions
  - How could a transition read from the world?
  - How can a transition do things to the world?


-}






{-
TODO:

- [x] Model our state!
- [x] Think about transitions
- [ ] Explore design choices
- [x] Make sure we can represent what we want! Step through states?
- [ ] Maybe some tests
    - [x] Can't get to invalid states?
    - [x] Can get to some expected states?

Things to think about:
  - [ ] Save increments and decrements
-}

init :: SomeState -> State
init = Open

close :: State -> Maybe State
close = \case
  Open a   -> Just (Closed a)
  _        -> Nothing

finalise :: State -> Maybe State
finalise = \case
  Closed a -> Just (Final a)
  Final a  -> Just (Final a)
  _        -> Nothing

{-
Properties!
  - [ ] Make sure all transitions move right
  - [ ] If we do enough (non-inc/dec) transitions we get to final; i.e. it terminates.
  - [x] Can always close and finalise any state.

  Stretch goal:
  - [ ] Finite # of decrements (given no increments being applied)
-}


isFinal :: Maybe State -> Property
isFinal (Just (Final _)) = property True
isFinal ms               = counterexample ("  ms expected final but got=" ++ show ms) $ property False

prop_canAlwaysCloseAndFinaliseAnyState :: State -> Property
prop_canAlwaysCloseAndFinaliseAnyState state =
  (isFinal (close state >>= finalise) & counterexample "close and finalise: ")
    .||. (isFinal (finalise state) & counterexample "finalise directly: ")

main :: IO ()
main = do
  hspec $ do
    it "can't go from init to finalise" $ do
      finalise (init emptyState) `shouldBe` Nothing
    it "can go to final from closed" $ do
      isFinal (close (init emptyState) >>= finalise)

    describe "properties" $ do
      it "Can always close and finalise any state"
        $ property prop_canAlwaysCloseAndFinaliseAnyState
