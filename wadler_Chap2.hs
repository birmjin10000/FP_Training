--ex.2.4.2
import Data.List

integers = 0:[ y | x<- [1..], y <- [x, -x]]

splitCandidates:: Int -> [(Int, Int)]
splitCandidates x = [(y, z) | z <- [-5..5], y <- integers, x == 10 * y + z]

split x =
	if (abs . snd . head $ candidate) == 5 then
		if (fst . head $ candidate) < (fst .last $ candidate) then
			head candidate
		else	last candidate
	else	head candidate
	where candidate = splitCandidate x
