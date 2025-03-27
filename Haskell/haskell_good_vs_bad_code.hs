-- Good Code: Using pattern matching for clarity and conciseness.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Bad Code:  Unnecessary use of guards and less readable.
factorial' :: Integer -> Integer
factorial' n
  | n == 0    = 1
  | otherwise = n * factorial' (n - 1)

-- Good Code:  Function composition for elegant solution.
composedFunction :: Int -> Int
composedFunction x = (*2) . (+1) $ x


-- Bad Code: verbose and less efficient.
composedFunction' :: Int -> Int
composedFunction' x = let y = x + 1 in y * 2


-- Good Code: Using Maybe type for handling potential errors.
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)


-- Bad Code:  Ignoring potential errors.  May cause runtime exceptions.
unsafeDivide :: Int -> Int -> Int
unsafeDivide x y = x `div` y

-- Good Code: Clear and concise data type definition.
data Person = Person { name :: String, age :: Int } deriving (Show)

-- Bad Code:  Overly complicated data type definition and naming.
data HumanBeing = MkHumanBeing { humanName :: String, humanAge :: Int } deriving (Show)
