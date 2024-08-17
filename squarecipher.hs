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
coordsToLetter :: LetterSquare -> Coords -> Char
coordsToLetter ltrsqr (Coords x y) = ltrsqr !! x !! y

-- Get coords of a letter in a LetterSquare
letterToCoords :: LetterSquare -> Char -> Coords
letterToCoords ltrsqr c = Coords row column
                   where Just row = findIndex (elem c) ltrsqr
                         Just column = findIndex (== c) $ ltrsqr !! row  

-- encode two letters (a digraph) A and B to make the
-- new digraph of: A+B and A-B
encodeDigraph :: LetterSquare -> (Char, Char) -> (Char, Char)
encodeDigraph ltrsqr (a, b) = (coordsToLetter ltrsqr (aCoords +$ bCoords), coordsToLetter ltrsqr (aCoords -$ bCoords)) 
                where aCoords = letterToCoords ltrsqr a
                      bCoords = letterToCoords ltrsqr b

-- decode a digraph X and Y as 3*(X+Y) and 3*(X-Y)
-- (this works because of group theory magic)
decodeDigraph :: LetterSquare -> (Char, Char) -> (Char, Char)
decodeDigraph ltrsqr (x, y) = (coordsToLetter ltrsqr ((Coords 3 3) *$ (xCoords +$ yCoords)), coordsToLetter ltrsqr ((Coords 3 3) *$ (xCoords -$ yCoords)))
                where xCoords = letterToCoords ltrsqr x
                      yCoords = letterToCoords ltrsqr y

-- silly silly silly
listToTuple [a,b] = (a,b)
tupleToList (a,b) = [a,b]

-- encodes a single word (as in no spaces), adding
-- an X at the end if it has an odd number of letters
encodeString :: LetterSquare -> String -> String
encodeString ltrsqr str = concat $ map (tupleToList . (\x -> encodeDigraph ltrsqr x) . listToTuple) digraphList 
                               -- add an "X" at the end if string has an odd number of letters 
                         where realStr = if (((`mod` 2) . length) str /= 0) then str ++ "X" else str
                               digraphList = chunksOf 2 realStr

-- decodes a single word (no spaces), almost the 
-- opposite of encodeString 
decodeString :: LetterSquare -> String ->  String
decodeString ltrsqr str = concat $ map (tupleToList . (\x -> decodeDigraph ltrsqr x) . listToTuple) digraphList
                          where digraphList = chunksOf 2 str

-- make uppercase, turn J into I, remove characters
-- that are not letters or spaces 
cleanString :: String -> String
cleanString str = map ((\x -> if x == 'J' then 'I' else x) . toUpper) $ filter (\x -> isLetter x || isSpace x) str 

-- encodes a string WITH spaces (note that this is done per-word,
-- so there will be encoded Xs after every odd-lengthed word) 
encode :: LetterSquare -> String -> String
encode ltrsqr str = (unwords . map (\x -> encodeString ltrsqr x) . words) realStr
                    where realStr = cleanString str

-- decodes a sentence with spaces
decode :: LetterSquare -> String -> String
decode ltrsqr str = (unwords . map (\x -> decodeString ltrsqr x) . words) str

-- encodes a string, discarding spaces for X-conomical reasons
encodeNoSpaces :: LetterSquare -> String -> String
encodeNoSpaces ltrsqr str = encodeString ltrsqr realStr
                            -- filter out spaces and clean string 
                            where realStr = filter (not . isSpace) $ cleanString str 
