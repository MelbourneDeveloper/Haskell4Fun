# Higher-Order Functions in Haskell

Higher-order functions are one of Haskell's most powerful features. They're functions that can take other functions as arguments or return functions as results. Don't worry if this sounds complex - we'll break it down step by step!

## What Are Higher-Order Functions?

A higher-order function is simply a function that either:
1. Takes a function as an argument
2. Returns a function as a result
3. Or both!

## The Most Common Higher-Order Functions

### map

`map` applies a function to every element in a list:

```haskell
-- Type signature
map :: (a -> b) -> [a] -> [b]

-- Examples
map double [1, 2, 3]          -- [2, 4, 6]
map not [True, False, True]   -- [False, True, False]

-- Using lambda functions
map (\x -> x + 1) [1, 2, 3]   -- [2, 3, 4]
```

### filter

`filter` keeps only the elements that satisfy a predicate (a function that returns Bool):

```haskell
-- Type signature
filter :: (a -> Bool) -> [a] -> [a]

-- Examples
filter even [1..10]           -- [2,4,6,8,10]
filter (>5) [1..10]          -- [6,7,8,9,10]

-- Using with custom predicates
isVowel c = c `elem` "aeiou"
filter isVowel "hello"        -- "eo"
```

### fold

`foldl` and `foldr` combine list elements using a function:

```haskell
-- Type signatures
foldl :: (b -> a -> b) -> b -> [a] -> b
foldr :: (a -> b -> b) -> b -> [a] -> b

-- Examples
sum = foldl (+) 0            -- Sum all numbers
product = foldl (*) 1        -- Multiply all numbers
concat = foldr (++) []       -- Join all lists

-- Custom fold example
countVowels = foldl (\acc c -> if c `elem` "aeiou" then acc + 1 else acc) 0
countVowels "hello"          -- 2
```

## Creating Your Own Higher-Order Functions

You can write functions that take other functions as parameters:

```haskell
-- Apply a function twice
twice :: (a -> a) -> a -> a
twice f x = f (f x)

twice (*2) 3        -- 12
twice reverse [1,2,3] -- [1,2,3]

-- Apply a function n times
applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = f (applyNTimes (n-1) f x)

applyNTimes 3 (*2) 1  -- 8
```

## Function Composition with Higher-Order Functions

Combine functions elegantly:

```haskell
-- Using the composition operator (.)
oddLength :: [a] -> Bool
oddLength = odd . length

-- Multiple compositions
sumOfSquares :: [Integer] -> Integer
sumOfSquares = sum . map (^2)

-- Example usage
oddLength "hello"     -- True
sumOfSquares [1,2,3]  -- 14
```

## Partial Application

In Haskell, you can partially apply functions to create new functions:

```haskell
-- Creating new functions through partial application
add :: Int -> Int -> Int
add x y = x + y

addFive :: Int -> Int
addFive = add 5

-- Using partial application with higher-order functions
map (add 5) [1,2,3]  -- [6,7,8]
```

## Real-World Examples

Here are some practical uses of higher-order functions:

```haskell
-- Processing a list of users
data User = User { name :: String, age :: Int }

-- Find all adult users
adults :: [User] -> [User]
adults = filter (\user -> age user >= 18)

-- Get all names in uppercase
upperNames :: [User] -> [String]
upperNames = map (map toUpper . name)

-- Calculate average age
averageAge :: [User] -> Double
averageAge users = fromIntegral (sum ages) / fromIntegral (length ages)
  where ages = map age users
```

## Common Patterns and Best Practices

1. **Chain Operations**
```haskell
-- Instead of nested functions
map f (filter p xs)

-- Use function composition
(map f . filter p) xs
```

2. **Point-Free Style**
```haskell
-- Instead of
f xs = map (*2) xs

-- Write
f = map (*2)
```

3. **Combine Higher-Order Functions**
```haskell
-- Find sum of even squares
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares = sum . filter even . map (^2)
```

## Practice Exercises

Try these exercises to master higher-order functions:

1. Write a function that applies a list of functions to a value:
```haskell
applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (\acc f -> f acc) x fs

-- Example:
applyAll [(*2), (+1), (^2)] 3  -- 49
```

2. Create a function that filters and transforms in one pass:
```haskell
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

-- Example:
filterMap even (*2) [1..5]  -- [4,8]
```

Remember:
- Higher-order functions make code more reusable and composable
- They often lead to more concise and readable solutions
- Practice combining them in different ways to solve complex problems

## Next Steps

Now that you understand higher-order functions, you can:
- Learn about monads and functors
- Explore more advanced type system features
- Study real-world applications of functional programming

Higher-order functions are fundamental to functional programming - they're your tools for building elegant, reusable solutions! 