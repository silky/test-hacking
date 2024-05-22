module Main where


import Prelude hiding (init)
import "hspec" Test.Hspec


data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)


{-
   TODO:

- [ ] Model our state!
- [ ] Think about transitions
- [ ] Explore design choices
- [ ] Make sure we can represent what we want! Step through states?
- [ ] Maybe some tests
    - [ ] Can't get to invalid states?
    - [ ] Can get to some expected states?

Things to think about:
  - [ ] Save increments and decrements
-}

init :: State
init = Open

close :: State -> Maybe State
close = \case
  Open -> Just Closed
  _    -> Nothing

finalise :: State -> Maybe State
finalise = \case
  Closed -> Just Final
  _      -> Nothing





-- Test make sure


main :: IO ()
main = do
  hspec $ do
    it "can't go from init to finalise" $ do
      finalise init `shouldBe` Nothing
    it "can go to final from closed" $ do
      (close init >>= finalise) `shouldBe` Just Final

