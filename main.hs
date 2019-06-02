import Control.Monad

main :: IO [()]
main = do
    winners <- forM optionsList (\(x,y) -> do
      putStrLn $ stringTupleToString (x,y)
      input <- getLine
      let winner  = if input == "1" then x else y
      let loser = if input == "1" then y else x
      return Event {winner=winner, loser=loser})
    mapM print winners

-- newtype Rankable = Rankable { name :: String } deriving Show

data Event = Event { winner :: String 
                  , loser :: String
                  } deriving Show

-- updateRank :: Rankable -> Rankable
-- updateRank a = Rankable "Chris"
stringTupleListToString :: [(String, String)] -> String
stringTupleListToString = concatMap stringTupleToString

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
testList = ["dog", "nintendo switch", "Dresser", "Meredith Workout Gear", "Chris Sneakers", "Blender", "Mattress Pad", "Sheet Set"]
-- testList = ["Dress", "Nintendo Switch"]
