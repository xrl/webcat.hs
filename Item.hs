module Item (
  Item(..)
) where
import Data.List

data Item = Item { steps   :: [Float],
                   loc     :: Float
                 } deriving (Show)

cumsteps Item{steps = s} = [0.0]  ++ (scanl1 (+) (init . tail $ s)) ++ [0.0]

itemResId item@Item{steps=s} atloc rating
      | length(s) < 2 = (0.0,0.0)
      | otherwise     = (res, res/sqrt(var))
      where (expected,var) = itemScoreVar item atloc
            res            = rating - expected

itemScoreVar item@Item{steps=s} atloc
      | length(s) < 2                         = (0.0,0.0)
      | otherwise                             = (expected,variance-expected**2)
      where solution       = 3
            indexes        = [1..(length(s)-2)]
            xs             = [1.0..]
            probs          = [itemRatingProb item atloc rating | rating <- indexes]
            ts             = zipWith (*) xs probs
            expected       = sum $ zipWith (+) xs ts
            variance       = sum $ zipWith (+) xs (zipWith (*) ts xs)

itemRatingProb item@Item{steps=s, loc=il} atloc rating
      | length(s) < 2                         = 0.0
      | otherwise                             = top / bottom
      where csteps                            = cumsteps item
            useful_csteps                     = (init . tail) (csteps)  -- (init . tail [0,1,4]) -> [1]
            d                                 = atloc - il
            top                               = exp(fromIntegral(rating)*d - (csteps !! rating))
            bottom                            = accWeightExp 0 useful_csteps
            numsteps                          = length(s)-1
            accWeightExp index []             = 1.0 + exp(fromIntegral(numsteps)*d)
            accWeightExp index (cstep:csteps) = exp(fromIntegral(index)*d+cstep) + (accWeightExp (index+1) csteps)