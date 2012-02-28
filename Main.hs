import Prelude
import Webcat.Factor (Factor(..),
                      FactorSession(..),
                      Item(..),
                      factorEstimate)
import Debug.Trace (trace)

main :: IO ()
main = do
       putStr $ show [verifyCurloc factor rs loc | tcase <- cases, let rs = fst tcase, let loc = snd tcase]

locs=[2.28,2.05,1.72,1.64,1.57,1.04,0.66,0.56,0.50,
      0.39,0.39,0.29,0.15,0.08,-0.29,-0.37,-0.45,-0.66,
      -0.80,-0.89,-1.21,-1.40,-1.93,-2.35,-3.0]

raw_steps = [0,-0.8,0.8]
raw_items = [Item raw_steps loc | loc <- locs]
factor = Factor{items = raw_items, starters = []}

cases = [(12,-1.558),(18,-0.768),(30,0.585),(38,1.578),(49,4.694)]
-- cases = [(18,-0.78)]

verifyCurloc f a e = ((trace ("diff: " ++ (show diff) ++ " rs: "
                  ++ (show a) ++ " eout: " ++ (show eout) ++ "\n") diff) <= 0.02)
	where 
		  diff = abs((eout)-e)
		  (eout,_,_,_) = trace (show fe) fe
		  fe = factorEstimate f fs
		  fs = FactorSession{queue=[],used=raw_items,maxsum=50,actsum=a,avgloc=0.0,se=0.0,curloc=0.0}

-- #134=>Item.new(134,"x", -2.240,"01",[0.0, 0.0]),
-- #137=>Item.new(137,"x", -4.300,"0112",[0.0, -0.70999999999999996, 0.70999999999999996]),
-- #138=>Item.new(138,"x", -0.970,"0112",[0.0, -0.13, 0.13]),
-- #139=>Item.new(139,"x", -0.460,"0112",[0.0, -0.82999999999999996, 0.82999999999999996])

-- f=Factor.new()

-- f.addobs(its[134],1)
-- f.addobs(its[137],4)
-- f.addobs(its[138],1)
-- f.addobs(its[139],4)

-- puts f.currentrasch()[0]
-- puts f.currentrasch()[1]
