-- Introduction to Functional Programming 1st edition Chapter 4.5 Calendar
import qualified Data.List as L

rjustify n s = space (n - length s) ++ s
space n = copy n ' '
copy n x = take n (repeat x)

height p = length p
width p = length (head p)

p `above` q
	| (width p == width q) = p ++ q
	| otherwise = [] 

p `beside` q
	| (height p == height q) = zipWith (++) p q
	| otherwise = []

stack = foldr1 above
spread = foldr1 beside

empty (height, width)
	| (height > 0 && width > 0) = copy height (copy width ' ')
	| otherwise = []

block n = stack . map spread . group n

group n xs = [take n (drop j xs) | j <- [0, n ..(length xs - n)]]

blockT n = spread . map stack . group n

topleft_frame (whole_height, whole_width) picture = 
    (picture `beside` empty (picture_height, whole_width - picture_width)) `above` empty (whole_height - picture_height, whole_width)
		where
			picture_height = height picture
			picture_width = width picture

display = unlines

calendar = display . block 3 . map picture . months

-----------------------------------------------------------------------------------------
-- Picturing a calendar

-- picture:: (a, b, b, b) -> []
picture (month, year, day_of_week_of_1st_day, length_of_month) = 
        (title month year) `above` (table day_of_week_of_1st_day length_of_month)

title month year = topleft_frame (2, 25) [month ++ " " ++ show year]

table day_of_week_of_1st_day length_of_month = 
        topleft_frame (8, 25) (daynames `beside` (entries day_of_week_of_1st_day length_of_month))

daynames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

entries day_of_week_of_1st_day length_of_month = blockT 7 (dates day_of_week_of_1st_day length_of_month)

dates day_of_week_of_1st_day length_of_month =
         map (date length_of_month) [1 - day_of_week_of_1st_day .. 42 - day_of_week_of_1st_day]

date length_of_month day
		| (day < 1 || length_of_month < day) = [rjustify 3 " "]
		| otherwise = [rjustify 3 (show day)]

-------------------------------------------------------------------------------------------
-- Building a calendar

-- months::(Num d, Integral a) => a -> [([Char], a, a, d)]
-- ex) ("JUNE", 2014, 0, 30)
months year = 
    L.zip4 month_names (copy 12 year) (day_of_week_of_every_1st_day_of_month year) (getLengthofMonth year)

month_names = 
    ["JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"]

getLengthofMonth year = [31,feb,31,30,31,30,31,31,30,31,30,31]
	where feb
		| (leap year) = 29
		| otherwise = 28

leap year
	| (year `mod` 100 == 0) = (year `mod` 400 == 0)
	| otherwise = (year `mod` 4 == 0)

-- 0: Sunday, 6: Saturday
day_of_week_of_january_1st year = (year + (year - 1) `div` 4 - (year - 1) `div` 100 + (year - 1) `div` 400) `mod` 7
-- Num a => a -> [a]
day_of_week_of_every_1st_day_of_month year = 
    take 12 (map (flip mod 7) (scanl (+) (day_of_week_of_january_1st year) (getLengthofMonth year)))

