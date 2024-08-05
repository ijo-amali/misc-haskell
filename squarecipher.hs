import Data.Char (isLetter, toUpper)
import Data.List (findIndex) 

key = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
Just ltsq = interpretKey key

type LetterSquare = [[Char]]
--type Coordinates = (Int, Int)

-- these are mod 5
data Coords = Coords Int Int deriving (Show, Eq) 

(+$) :: Coords -> Coords -> Coords
(Coords a b) +$ (Coords x y) = Coords ((a + x) `mod` 5) ((b + y) `mod` 5)

(-$) :: Coords -> Coords -> Coords
(Coords a b) -$ (Coords x y) = Coords ((a - x) `mod` 5) ((b - y) `mod` 5)  

(*$) :: Coords -> Coords -> Coords
(Coords a b) *$ (Coords x y) = Coords ((a * x) `mod` 5) ((b * y) `mod` 5)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
                where (ys, zs) = splitAt n xs

interpretKey :: String -> Maybe LetterSquare
interpretKey str
    | length str /= 25           = Nothing
    | not $ all isLetter str     = Nothing
    | otherwise                  = Just ltrsqr
      where ltrsqr = chunksOf 5 str 

coordsToLetter :: Coords -> LetterSquare -> Char
coordsToLetter (Coords x y) ltrsqr = ltrsqr !! x !! y

letterToCoords :: Char -> LetterSquare -> Coords
letterToCoords c ltrsqr = Coords row column
                   where Just row = findIndex (elem c) ltrsqr
                         Just column = findIndex (== c) $ ltrsqr !! row  

encodeDigraph :: (Char, Char) -> LetterSquare -> (Char, Char)
encodeDigraph (a, b) ltrsqr = (coordsToLetter (aCoords +$ bCoords) ltrsqr, coordsToLetter (aCoords -$ bCoords) ltrsqr) 
                where aCoords = letterToCoords a ltrsqr
                      bCoords = letterToCoords b ltrsqr

decodeDigraph :: (Char, Char) -> LetterSquare -> (Char, Char)
decodeDigraph (x, y) ltrsqr = (coordsToLetter ((Coords 3 3) *$ (xCoords +$ yCoords)) ltrsqr, coordsToLetter ((Coords 3 3) *$ (xCoords -$ yCoords)) ltrsqr)
                where xCoords = letterToCoords x ltrsqr
                      yCoords = letterToCoords y ltrsqr

listToTuple [a,b] = (a,b)
tupleToList (a,b) = [a,b]

encodeString :: String -> LetterSquare -> String
encodeString str ltrsqr = concat $ map (tupleToList . (\x -> encodeDigraph x ltrsqr) . listToTuple) digraphList 
                               -- add an "X" at the end if string has an odd number of letters 
                         where realStr = if (((`mod` 2) . length) str /= 0) then str ++ "X" else str
                               digraphList = chunksOf 2 realStr

decodeString :: String -> LetterSquare -> String
decodeString str ltrsqr = concat $ map (tupleToList . (\x -> decodeDigraph x ltrsqr) . listToTuple) digraphList
                          where digraphList = chunksOf 2 str



--home stretch bitches

encode :: String -> LetterSquare -> String
encode str ltrsqr = (unwords . map (\x -> encodeString x ltrsqr) . words) strUpper
                    where strUpper = map toUpper str

decode :: String -> LetterSquare -> String
decode str ltrsqr = (unwords . map (\x -> decodeString x ltrsqr) . words) str
