module Item (
  Item(..)
) where
import Data.List

data Item = Item { steps  :: [Int],
                   curloc :: Int
                 } deriving (Show)

nsteps  Item {steps = s}  = length(s)
nsteps1 Item {steps = s}  = length(s)-1

cumsteps Item {steps = s} = [0] ++ cumstepAccum(s) ++ [0]

-- I don't need the sum of the list but this will work.
cumstepAccum list         = snd(accumulated)
       where accumulated  = mapAccumL cumstepAccumFunc 0 $ middle list
             middle       = init . tail
cumstepAccumFunc acc entry = (acc+entry,acc+entry)
