--ex.2.4.2
split::Int -> (Int, Int)
split x =
	let 	y = x `mod` 10
		z = x `div` 10
	in	(if abs(y) > 5 then
			if 	y > 5 then (y - 10, z + 1)
			else	(10 + y, z - 1)
		else	(y, z))
