import Data.List

--ex.2.4.1
age::(Int, Int, Int) -> (Int, Int, Int) -> Int
age birthday today =
	if (length diff == 3 || (length diff == 2 && diff!!1 > 0)) then diff!!0
	else	max (diff!!0 - 1) 0
	where diff = takeWhile (>=0) $ reverse (zipWith (-) (tripleToList today) (tripleToList birthday))

tripleToList::(Int, Int, Int) -> [Int]
tripleToList (a, b, c) = a:b:c:[]


--ex.2.4.2
splitCandidates:: Int -> [(Int, Int)]
splitCandidates x = [(y, z) | y <- [-5..5], z <- [-(abs(x) `div` 10)-1..(abs(x) `div` 10)+1], x == y + 10 * z]

split x =
	if length candidate > 1 then
		if (abs . snd . head $ candidate) < (abs . snd .last $ candidate) then
			head candidate
		else	last candidate
	else	head candidate
	where candidate = splitCandidates x
