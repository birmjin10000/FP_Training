type State = (Direction, Pen, Point)
type Direction = Int 
type Pen = Bool
type Point = (Int, Int)
type Command = State -> State

-- North(0), East(1), South(2), West(3)
move::Command
move (0, pen, (x, y)) = (0, pen, (x - 1, y))
move (1, pen, (x, y)) = (1, pen, (x, y + 1))
move (2, pen, (x, y)) = (2, pen, (x + 1, y))
move (3, pen, (x, y)) = (3, pen, (x, y - 1))

-- turn left/right
right::Command
right (direction, pen, point) = ((direction + 1) `mod` 4, pen, point)
left::Command
left (direction, pen, point) = ((direction + 3) `mod` 4, pen, point)

-- pen up/down
up::Command
up (direction, _, point) = (direction, False, point)
down::Command
down (direction, _, point) = (direction, True, point)

-- "turtle" takes a sequence of commands, returns a sequence of states
turtle::[Command]->[State]
turtle = scanl applyto (0, False, (0,0))
applyto x f = f x


--display::[Command]->[Char]
display = layout . picture . trail . turtle

-- "trail" produces a list of points
trail::[State] -> [Point]
trail [] = []
trail ((_, pen, point):xs)
		| (pen == True) = point:(trail xs)
		| otherwise = trail xs

{- 
"layout" flattens the picture into a list of characters 
by inserting newlines between rows and concatenating the results 
-}
layout [] = []
layout (x:xs) = x ++ "\n" ++ (layout xs)


-- "picture" converts the list into a two-dimensional picture ( a value of type [[Char]] )
picture::[Point] -> [[Char]]
picture xs = symbolise $ bitmap xs
bitmap ps = [[elem (x, y) ps | y <- yran] | x <- xran]
		where 
			xran = range (map fst ps)
			yran = range (map snd ps)
range xs = [minimum xs .. maximum xs]

-- "boolstr" converts each true-value to a suitable string of characters 
boolchar x
		| (x == True) = 'T'
		| otherwise = ' '

boolstr = map boolchar 
symbolise = map boolstr

