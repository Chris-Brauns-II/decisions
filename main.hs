import Control.Monad
import Data.List

main :: IO [()]
main = do
    results <- forM optionsList (\tuple -> do
      putStrLn $ stringTupleToString tuple
      input <- getLine
      return $ eventFromInput input tuple)
    mapM (print . name) $ sortBy (flip compare) $ eventsToRankables results []

data Rankable = Rankable { name :: String
                          , wins :: Int
                          , losses :: Int
                          } deriving (Show, Eq)

instance Ord Rankable where
  compare a b = compare (winLossRatio a) (winLossRatio b)

data Event = Event { winner :: String 
                  , loser :: String
                  } deriving Show

eventFromInput :: String -> (String, String) -> Event
eventFromInput input (x, y) | input == "1" = Event { winner = x, loser = y }
                            | input == "2" = Event { winner = y, loser = x }
                            | otherwise = error "Not a valid choice"

winLossRatio :: Rankable -> Double
winLossRatio rankable | losses rankable == 0 && wins rankable == 0 = 0
                      | losses rankable == 0 = 1
                      | otherwise = fromIntegral (wins rankable) / (fromIntegral (wins rankable) + fromIntegral (losses rankable))

eventsToRankables :: [Event] -> [Rankable] -> [Rankable]
eventsToRankables [] [] = []
eventsToRankables [] rankables = rankables
eventsToRankables (x:xs) [] = eventsToRankables xs (updateBoth [] x)
eventsToRankables (x:xs) rankables = eventsToRankables xs (updateBoth rankables x)

updateBoth :: [Rankable] -> Event -> [Rankable]
updateBoth rankables event = updateLoser 
                              (updateWinner 
                                rankables 
                                (winner event))
                              (loser event)

updateWinner :: [Rankable] -> String -> [Rankable]
updateWinner [] w = [Rankable { name = w, wins = 1, losses = 0 }]
updateWinner (x:xs) w | name x == w = incrementWins x : xs
                      | otherwise = x : updateWinner xs w

updateLoser :: [Rankable] -> String -> [Rankable]
updateLoser [] l = [Rankable { name = l, wins = 0, losses = 1 }]
updateLoser (x:xs) l | name x == l = incrementLosses x : xs
                     | otherwise = x : updateLoser xs l

incrementWins :: Rankable -> Rankable
incrementWins x = Rankable { name = name x, wins = wins x + 1, losses = losses x }

incrementLosses :: Rankable -> Rankable
incrementLosses x = Rankable { name = name x, wins = wins x, losses = losses x + 1 }

stringTupleToString :: (String, String) -> String
stringTupleToString (x, y) = "Compare:\n  1: " ++ x ++ "\n  2: " ++ y ++ "\n"

commutativelyEqual :: (Eq a) => (a, a) -> (a, a) -> Bool
commutativelyEqual a (x, y) = a == (x, y) || a == (y, x)

removeDuplicates :: (Eq a) => [(a, a)] -> [(a, a)]
removeDuplicates [] = []
removeDuplicates (x:xs) | any (commutativelyEqual x) xs = removeDuplicates xs
                        | otherwise = x : removeDuplicates xs

optionsList :: [(String, String)]
optionsList = removeDuplicates [(x, y) | x <- testList, y <- testList, y /= x]

testList :: [String]
-- testList = ["dog", "Nintendo Switch", "Meredith Workout Gear", "Chris Sneakers", "Blender", "Bedroom Stuff"]
testList = ["Dog", "Nintendo Switch", "Bedroom Stuff"]
