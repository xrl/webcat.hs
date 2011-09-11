import Prelude
import Factor (Factor(..),
               FactorSession(..),
               Item(..),
               factorEstimate)


main :: IO ()
main = do
       putStr "hi"

locs=[2.28,2.05,1.72,1.64,1.57,1.04,0.66,0.56,0.50,
          0.39,0.39,0.29,0.15,0.08,-0.29,-0.37,-0.45,-0.66,
          -0.80,-0.89,-1.21,-1.40,-1.93,-2.35,-3.0]

raw_steps = [0,-0.8,0.8]
raw_items = [Item raw_steps loc | loc <- locs]
factor = Factor{items = raw_items, starters = []}