module Webcat.Factor(
	Factor(..),
	FactorSession(..),
	factorEstimate,
	Item(..)
) where
import Webcat.Item as Item
import Debug.Trace (trace)

data Factor = Factor {
	items    :: [Item],
	starters :: [Item]
} deriving (Show)

data FactorSession = FactorSession{
	queue :: [Item],
	used  :: [Item],
	maxsum :: Double,
	actsum :: Double,
	avgloc :: Double,
	se     :: Double,
	curloc :: Double
} deriving (Show)

createSession f@Factor{starters=s} = FactorSession{queue=s,
												   used=[],
												   maxsum=0,
												   actsum=0,
												   avgloc=0.0,
												   se=0.0,
												   curloc=0.0}
factorEstimate
  :: Factor -> FactorSession -> (Double, Double, Double, Double)
factorEstimate f@Factor{starters=s} fs@FactorSession{used = u, actsum = as, curloc=old}
	| nused >= nstarters  = estimate
	| otherwise           = quickEstimate
	where 
		  nused         = length(u)
		  nstarters     = length(s)
		  estimate      = fullFacEst f fs as 1.0 0.0
		  quickEstimate = (0.0,0.0,0,0.0) -- quickFacEst f fs as 

quickFacEst f@Factor{} fs@FactorSession{maxsum = ms, avgloc=al, used =u} dr
	| otherwise =  0.0 -- lhs+rhs
	where 
		  r = factorCleanRaw fs dr
		  lhs = log(r/(ms-r))
		  rhs = al / fromIntegral(nused)
		  nused = length(u)

-- c'est quoi r?
-- un peut mysterieux: enleve le "se" def et ca fait encore compiling
-- un autre mystery: bouge 'interval =' apres le 'where'
fullFacEst f@Factor{} fs@FactorSession{} dirtyRaw x xold
	| interval >= 0.00000001	 = 
	                           (fullFacEst f fs r xnext x)
	| otherwise          = trace "done with fullFacEst\n" (xnext,se,niter,fisher)
	where 
		  interval = abs (x-xold)
		  xnext = xold - ((est-r)/(var))
		  scorevar = factorScoreVar f fs xold
		  loc
		    | interval < 1.0 = x
		    | x < xold       = xold - 1.0
		    | otherwise      = xold + 1.0
		  est = fst ((trace ("loc: " ++ (show loc) ++ " scorevar: " ++ (show scorevar) ++ "\n")) scorevar)
		  var = snd scorevar
		  se  = 1.0/sqrt(var)
		  niter = 0
		  fisher = var
		  r = factorCleanRaw fs dirtyRaw

factorScoreVar f@Factor{} fs@FactorSession{used=u} atloc
	| length(u) == 0 = (0.0,0.0)
	| otherwise      = (score,var)
	where scorevars = [itemMeanVar i atloc | i <- u]
	      score     = sum [fst tup | tup <- scorevars]
	      var       = sum [snd tup | tup <- scorevars]

factorCleanRaw fs@FactorSession{maxsum = ms} dirtyRaw
	| dirtyRaw < 1.0		= 0.5
	| dirtyRaw > ms 		= ms - 0.5
	| otherwise		        = dirtyRaw
