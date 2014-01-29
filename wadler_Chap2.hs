--ex.2.4.2
import Data.List

splitCandidates:: Int -> [(Int, Int)]
splitCandidates x = [(y, z) | y <- [-5..5], z <- [-(abs(x) `div` 10)-1..(abs(x) `div` 10)+1], x == y + 10 * z]

split x =
	if length candidate == 5 then
		if (snd . head $ candidate) < (snd .last $ candidate) then
			head candidate
		else	last candidate
	else	head candidate
	where candidate = splitCandidates x
