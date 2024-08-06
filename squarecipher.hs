import Data.Char (isLetter, isSpace, toUpper)
import Data.List (findIndex) 

defaultKey = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
Just defaultLtrsqr = interpretKey defaultKey

-- Just a type synonym to make the function types more readable 
type LetterSquare = [[Char]]

-- Coordinates for the letters in the letter squares, mod 5
data Coords = Coords Int Int deriving (Show, Eq) 

-- Add function for coords, mod 5
(+$) :: Coords -> Coords -> Coords
(Coords a b) +$ (Coords x y) = Coords ((a + x) `mod` 5) ((b + y) `mod` 5)
-- Subtract function for coords, mod 5
(-$) :: Coords -> Coords -> Coords
(Coords a b) -$ (Coords x y) = Coords ((a - x) `mod` 5) ((b - y) `mod` 5)  
-- Multiply functino for coords, mod 5
(*$) :: Coords -> Coords -> Coords
(Coords a b) *$ (Coords x y) = Coords ((a * x) `mod` 5) ((b * y) `mod` 5)

-- Splits a list into chunks of length n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
                where (ys, zs) = splitAt n xs

-- Takes a string and returns Just LetterSquare if it's the right
-- size and is made of only letters, otherwise Nothing
interpretKey :: String -> Maybe LetterSquare
interpretKey str
    | length str /= 25           = Nothing
    | not $ all isLetter str     = Nothing
    | otherwise                  = Just ltrsqr
      where ltrsqr = chunksOf 5 $ map toUpper str 

-- Lookup letter in LetterSquare from coords
coordsToLetter :: Coords -> LetterSquare -> Char
coordsToLetter (Coords x y) ltrsqr = ltrsqr !! x !! y

-- Get coords of a letter in a LetterSquare
letterToCoords :: Char -> LetterSquare -> Coords
letterToCoords c ltrsqr = Coords row column
                   where Just row = findIndex (elem c) ltrsqr
                         Just column = findIndex (== c) $ ltrsqr !! row  

-- encode two letters (a digraph) A and B to make the
-- new digraph of: A+B and A-B
encodeDigraph :: (Char, Char) -> LetterSquare -> (Char, Char)
encodeDigraph (a, b) ltrsqr = (coordsToLetter (aCoords +$ bCoords) ltrsqr, coordsToLetter (aCoords -$ bCoords) ltrsqr) 
                where aCoords = letterToCoords a ltrsqr
                      bCoords = letterToCoords b ltrsqr

-- decode a digraph X and Y as 3*(X+Y) and 3*(X-Y)
-- (this works because of group theory magic)
decodeDigraph :: (Char, Char) -> LetterSquare -> (Char, Char)
decodeDigraph (x, y) ltrsqr = (coordsToLetter ((Coords 3 3) *$ (xCoords +$ yCoords)) ltrsqr, coordsToLetter ((Coords 3 3) *$ (xCoords -$ yCoords)) ltrsqr)
                where xCoords = letterToCoords x ltrsqr
                      yCoords = letterToCoords y ltrsqr

-- silly silly silly
listToTuple [a,b] = (a,b)
tupleToList (a,b) = [a,b]

-- encodes a single word (as in no spaces), adding
-- an X at the end if it has an odd number of letters
encodeString :: String -> LetterSquare -> String
encodeString str ltrsqr = concat $ map (tupleToList . (\x -> encodeDigraph x ltrsqr) . listToTuple) digraphList 
                               -- add an "X" at the end if string has an odd number of letters 
                         where realStr = if (((`mod` 2) . length) str /= 0) then str ++ "X" else str
                               digraphList = chunksOf 2 realStr

-- decodes a single word (no spaces), almost the 
-- opposite of encodeString 
decodeString :: String -> LetterSquare -> String
decodeString str ltrsqr = concat $ map (tupleToList . (\x -> decodeDigraph x ltrsqr) . listToTuple) digraphList
                          where digraphList = chunksOf 2 str

-- make uppercase, turn J into I, remove characters
-- that are not letters or spaces 
cleanString :: String -> String
cleanString str = map ((\x -> if x == 'J' then 'I' else x) . toUpper) $ filter (\x -> isLetter x || isSpace x) str 

-- encodes a string WITH spaces (note that this is done per-word,
-- so there will be encoded Xs after every odd-lengthed word) 
encode :: String -> LetterSquare -> String
encode str ltrsqr = (unwords . map (\x -> encodeString x ltrsqr) . words) realStr
                    where realStr = cleanString str

-- decodes a sentence with spaces
decode :: String -> LetterSquare -> String
decode str ltrsqr = (unwords . map (\x -> decodeString x ltrsqr) . words) str

-- encodes a string, discarding spaces for X-conomical reasons
encodeNoSpaces :: String -> LetterSquare -> String
encodeNoSpaces str ltrsqr = encodeString realStr ltrsqr
                            -- filter out spaces and clean string 
                            where realStr = filter (not . isSpace) $ cleanString str 
