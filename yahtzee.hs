import Control.Monad
import Data.Char
import Data.List
import System.Random
import Text.Printf

data Die = Ace | Two | Three | Four | Five | Six deriving (Eq, Enum, Ord)
instance Show Die where
    show Ace   = [chr 9856]
    show Two   = [chr 9857]
    show Three = [chr 9858]
    show Four  = [chr 9859]
    show Five  = [chr 9860]
    show Six   = [chr 9861]

data Box = Aces | Twos | Threes | Fours | Fives | Sixes | ThreeOfAKind | FourOfAKind | FullHouse | SmallStraight | LargeStraight | Yahtzee | Chance deriving (Eq, Enum)
instance Show Box where
    show Aces          = "Aces   " ++ show Ace   ++ " = 1"
    show Twos          = "Twos   " ++ show Two   ++ " = 2"
    show Threes        = "Threes " ++ show Three ++ " = 3"
    show Fours         = "Fours  " ++ show Four  ++ " = 4"
    show Fives         = "Five   " ++ show Five  ++ " = 5"
    show Sixes         = "Sixes  " ++ show Six   ++ " = 6"
    show ThreeOfAKind  = "3 of a kind "
    show FourOfAKind   = "4 of a kind "
    show FullHouse     = "Full House  "
    show SmallStraight = "Sm. Straight"
    show LargeStraight = "Lg. Straight"
    show Yahtzee       = "YAHTZEE     "
    show Chance        = "Chance      "

toInt :: Die -> Int
toInt = (+ 1) . fromEnum

fromInt :: Int -> Die
fromInt = toEnum . (subtract 1)

prettyRoll :: [Die] -> [Char]
prettyRoll xs = foldl (\acc x -> acc ++ x ++ " ") "" (map show xs)

prettyBoxScore :: (Box, Int) -> [Char]
prettyBoxScore (box, score) = "| " ++ show box ++ " | " ++ (printf "%2d" score) ++ " | "

prettyScoreSheet :: [(Box, Int)] -> [Char]
prettyScoreSheet xs = foldl (\acc x -> acc ++ x ++ "\n") "" (map prettyBoxScore xs)

total :: [Die] -> Int
total xs = sum $ map toInt xs

score :: Box -> [Die] -> Int
score Aces xs = total $ filter (== Ace) xs
score Twos xs = total $ filter (== Two) xs
score Threes xs = total $ filter (== Three) xs
score Fours xs = total $ filter (== Four) xs
score Fives xs = total $ filter (== Five) xs
score Sixes xs = total $ filter (== Six) xs
score Chance xs = total xs

score ThreeOfAKind xs 
    | isThreeOfAKind xs = total xs
    | otherwise = 0

score FourOfAKind xs
    | isFourOfAKind xs = total xs
    | otherwise = 0

score FullHouse xs
    | isFullHouse xs = 25
    | otherwise = 0

score SmallStraight xs
    | isSmallStraight xs = 30
    | otherwise = 0

score LargeStraight xs
    | isLargeStraight xs = 40
    | otherwise = 0

score Yahtzee xs
    | isYahtzee xs = 50
    | otherwise = 0

scoreAll :: [Die] -> [(Box, Int)]
scoreAll xs = map (\box -> (box, score box xs)) [Aces ..]

groupCounts :: [Die] -> [Int]
groupCounts xs = sort $ map length $ group . sort $ xs

isFullHouse :: [Die] -> Bool
isFullHouse xs = groupCounts xs == [2,3]

isNumOfAKind :: Int -> [Die] -> Bool
isNumOfAKind num xs = foldl (\acc ys -> if length ys >= num then True else acc) False (group . sort $ xs)

isThreeOfAKind :: [Die] -> Bool
isThreeOfAKind = isNumOfAKind 3

isFourOfAKind :: [Die] -> Bool
isFourOfAKind = isNumOfAKind 4

isYahtzee :: [Die] -> Bool
isYahtzee = isNumOfAKind 5

isSequence :: [Die] -> Bool
isSequence [] = True
isSequence (x:[]) = True
isSequence (Six:xs) = False
isSequence (x:y:zs) 
    | y == succ x = isSequence $ y:zs
    | otherwise = False

isStraight :: Int -> [Die] -> Bool
isStraight num xs = or [not (null ys) | ys <- subsequences $ sort xs, length ys == num, isSequence ys]

isSmallStraight :: [Die] -> Bool
isSmallStraight = isStraight 4

isLargeStraight :: [Die] -> Bool
isLargeStraight = isStraight 5

roll :: (RandomGen g) => g -> [Die]
roll generator = map fromInt $ take 5 $ randomRs (1, 6) generator

-- Demonstrate some I/O
main = do
    gen <- getStdGen
    let myRoll = roll gen
    putStrLn $ "You rolled: " ++ (prettyRoll myRoll)
    putStrLn $ "Possible scores:\n" ++ (prettyScoreSheet $ scoreAll myRoll)
    