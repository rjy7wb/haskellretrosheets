import System.IO
import Data.Maybe
import Data.List
import Data.List.Split

type Year = Int
type Month = Int
type Day = Int
type Date = (Day,Month,Year)

data Park = Park {
    parkID  :: String,
    name    :: String,
    altName :: String,
    city    :: String,
    state   :: String,
    start   :: Date,
    end     :: Date,
    league  :: String,
    notes   :: String
    } deriving (Show)

parseDate :: String -> Maybe Date
parseDate [] = Just (1,6,2020)
parseDate xs = parseDate' ((splitOn "/") xs)
    where
        parseDate' (x:y:z:zs) = Just ((read x :: Int),(read y :: Int),(read z :: Int))

parseLine :: String -> Maybe Park
parseLine [] = Nothing
parseLine xs = parseLine' (splitOn "," xs)
        where parseLine' (a:b:c:d:e:f:g:h:i:is) = Just (Park a b c d e (fromJust $ parseDate f) (fromJust $ parseDate g) h i)

getData :: IO [Park]
getData = do
    handle <- openFile "parkcode.txt" ReadMode
    contents <- hGetContents handle  
    let byLine = lines contents
    let parkList = map (fromJust . parseLine) (tail byLine)

    return parkList

main = do
    parkList <- getData
    return parkList