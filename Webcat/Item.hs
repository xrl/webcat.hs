module Webcat.Item (
  Item(..),
  itemMeanVar
) where
import Data.List

data Item = Item { steps   :: [Double],
                   loc     :: Double
                 } deriving (Show)

itemMeanVar item@Item{steps=s} atloc = (sum expected,sum variance - (sum expected)**2)
      where xs             = [1.0..]
            probs          = [itemProb item atloc rating | rating <- [1..(length(s)-1)]]
            expected       = zipWith (*) xs probs
            variance       = zipWith (*) expected xs

itemProb item@Item{steps=s, loc=l} atloc rating
      | length(s) < 2 = top / bottom
      | otherwise     = top / (bottom + extra)
      where top = exp(fromIntegral(rating) * d - cstep)
            bottom = 1.0 + exp(fromIntegral(nsteps1) * d)
            d = atloc - l
            csteps = cumsteps s
            cstep = csteps !! rating
            nsteps1 = fromIntegral(length(s)-1)
            extra = sum [exp(fromIntegral(h)*d - cp) | h <- [1..(nsteps1-1)], let cp = (csteps !! h)] 

-- DRAGONS
cumsteps steps = snd $ mapAccumL (\acc x -> (acc+x,acc+x)) 0 steps

--item1 = Item [0.0,-1.0,1.0] 1.0
--itemMeanVar item1 1.0 == (0.5950684074995565,0.36771987456157346)
--itemMeanVar item1 0.0 == (1.0,0.4238831152341711)
