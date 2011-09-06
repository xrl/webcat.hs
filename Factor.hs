module Factor(
	Factor(..),
	FactorSession(..),
	factorEstimate,
	secret
) where
import Item

secret = "HI"

data Factor = Factor {
	items    :: [Item],
	startloc :: Float,
	starters :: [Item]
} deriving (Show)

data FactorSession = FactorSession{
	queue :: [Item],
	used  :: [Item],
	maxsum :: Float,
	actsum :: Float,
	avgloc :: Float,
	se     :: Float,
	curloc :: Float
} deriving (Show)

factorEstimate f@Factor{} fs@FactorSession{used = u, actsum = as} rating old
	| nused >= 3  = estimate
	| otherwise   = quickEstimate
	where 
		  nused         = length(u)
		  estimate      = fullFacEst  f fs as 0.0 0.0
		  quickEstimate = 0.0 -- quickFacEst f fs as 

quickFacEst f@Factor{} fs@FactorSession{maxsum = ms, avgloc=al, used =u} dr
	| otherwise =  lhs+rhs
	where 
		  r = factorCleanRaw fs dr
		  lhs = log(r/(ms-r))
		  rhs = al / fromIntegral(nused)
		  nused = length(u)

-- c'est quoi r?
-- un peut mysterieux: enleve le "se" def et ca fait encore compiling
-- un autre mystery: bouge 'interval =' apres le 'where'
fullFacEst f@Factor{} fs@FactorSession{} dirtyRaw x xold
	| interval >= 0.01	 = fullFacEst f fs r xnext x
	| otherwise          = (xnext,se,niter,fisher)
	where 
		  interval = abs (x-xold)
		  xnext = xold - ((est-r)/(var))
		  scorevar = factorScoreVar f fs loc
		  loc
		    | interval < 1.0 = x
		    | x < xold       = xold - 1.0
		    | otherwise      = xold + 1.0
		  est = fst scorevar
		  var = snd scorevar
		  se  = 1.0/sqrt(var)
		  niter = 0.0
		  fisher = var
		  r = factorCleanRaw fs dirtyRaw

factorScoreVar f@Factor{} fs@FactorSession{used=u} atloc
	| length(u) == 0 = (0.0,0.0)
	| otherwise      = (score,var)
	where scorevars = [itemScoreVar i atloc | i <- u]
	      score     = sum [fst tup | tup <- scorevars]
	      var       = sum [snd tup | tup <- scorevars]

factorCleanRaw fs@FactorSession{maxsum = ms} dirtyRaw
	| dirtyRaw < 1.0		= 0.5
	| dirtyRaw > ms 		= ms - 0.5
	| otherwise		        = dirtyRaw