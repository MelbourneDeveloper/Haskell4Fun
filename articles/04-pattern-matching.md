# Pattern Matching in Haskell

Pattern matching is a fundamental feature in Haskell that enables declarative deconstruction of data types. It's a powerful mechanism for expressing computation through equations and case analysis.

## Table of Contents
- [Basic Pattern Matching](#basic-pattern-matching)
- [Pattern Matching on Lists](#pattern-matching-on-lists)
- [Guards with Patterns](#guards-with-patterns)
- [Record Patterns](#record-patterns)
- [Irrefutable Patterns](#irrefutable-patterns)
- [View Patterns](#view-patterns)
- [Pattern Matching Best Practices](#pattern-matching-best-practices)
- [Common Pitfalls](#common-pitfalls)

## Basic Pattern Matching

Pattern matching combines binding and control flow:

```haskell
-- Pattern matching on values
isZero :: (Eq a, Num a) => a -> Bool
isZero 0 = True
isZero _ = False

-- Pattern matching on tuples
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Pattern matching on custom types
data Shape 
    = Circle Double 
    | Rectangle Double Double
    | Triangle Double Double Double
    deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = 
    let s = (a + b + c) / 2  -- semi-perimeter
    in sqrt (s * (s - a) * (s - b) * (s - c))  -- Heron's formula
```

## Pattern Matching on Lists

Lists support rich pattern matching capabilities:

```haskell
-- Basic list patterns with Maybe for safety
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- Pattern matching with list comprehension
pairs :: [a] -> [(a, a)]
pairs (x:y:rest) = (x,y) : pairs rest
pairs _          = []

-- Multiple patterns with increasing specificity
describe :: [a] -> String
describe []         = "Empty list"
describe [_]        = "Singleton"
describe [_, _]     = "Pair"
describe (_:_:_:xs) = "Long list with " ++ show (length xs + 3) ++ " elements"
```

## Guards with Patterns

Guards extend pattern matching with boolean conditions:

```haskell
data Temperature = Temperature 
    { celsius :: Double
    } deriving (Show, Eq, Ord)

classify :: Temperature -> String
classify (Temperature c)
    | c < 0     = "Freezing"
    | c < 10    = "Cold"
    | c < 20    = "Mild"
    | c < 30    = "Warm"
    | c < 40    = "Hot"
    | otherwise = "Extreme heat"

-- Pattern matching with complex guards
gcd' :: (Integral a) => a -> a -> a
gcd' x y
    | y == 0    = abs x
    | x == 0    = abs y
    | otherwise = gcd' y (x `mod` y)
```

## Record Patterns

Record patterns provide field access and matching:

```haskell
data Person = Person 
    { name    :: String
    , age     :: Int
    , address :: Address
    } deriving (Show, Eq)

data Address = Address 
    { street  :: String
    , city    :: String
    , country :: String
    } deriving (Show, Eq)

-- Nested record pattern matching
formatAddress :: Person -> String
formatAddress Person{name = n, address = Address{..}} =
    n ++ " lives at " ++ street ++ ", " ++ city ++ ", " ++ country

-- Record update with pattern matching
birthday :: Person -> Person
birthday p@Person{age = a} = p{age = a + 1}
```

## Irrefutable Patterns

Lazy patterns (irrefutable patterns) using `~`:

```haskell
-- Irrefutable pattern with ~
lazyPattern :: Bool -> (Int, Int) -> Int
lazyPattern True  ~(x, _) = x
lazyPattern False ~(_, y) = y

-- Useful for lazy evaluation
data Infinite a = Cons a (Infinite a)

head' :: Infinite a -> a
head' ~(Cons x _) = x  -- Safe because Infinite is always non-empty
```

## View Patterns

View patterns transform data before matching:

```haskell
{-# LANGUAGE ViewPatterns #-}

-- Simple view pattern
isEven :: Integral a => a -> Bool
isEven ((`mod` 2) -> 0) = True
isEven _                = False

-- Complex view pattern example
data Time = Time Int  -- Minutes since midnight

timeComponents :: Time -> (Int, Int)
timeComponents (Time t) = (t `div` 60, t `mod` 60)

showTime :: Time -> String
showTime t@(timeComponents -> (h, m)) =
    show h ++ ":" ++ (if m < 10 then "0" else "") ++ show m
```

## Pattern Matching Best Practices

### 1. Exhaustiveness

Always ensure complete pattern coverage:

```haskell
-- Good: total function
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Bad: partial function
unsafeDiv :: Integral a => a -> a -> a
unsafeDiv x y = x `div` y  -- Crashes on division by zero
```

### 2. Specificity Order

Order patterns from specific to general:

```haskell
-- Good: correct ordering
listLength :: [a] -> String
listLength []     = "empty"
listLength [_]    = "singleton"
listLength [_,_]  = "pair"
listLength _      = "longer"

-- Bad: unreachable patterns
badLength :: [a] -> String
badLength (_:_)  = "non-empty"  -- Catches everything but []
badLength []     = "empty"
badLength [_]    = "singleton"  -- Never reached!
```

### 3. Use Type System

Let the type system guide pattern matching:

```haskell
-- Good: type-safe pattern matching
data Option a = Some a | None

mapOption :: (a -> b) -> Option a -> Option b
mapOption _ None     = None
mapOption f (Some x) = Some (f x)

-- Better than Maybe for custom semantics
data Validation e a = Error e | Success a

validateAge :: Int -> Validation String Int
validateAge n
    | n < 0     = Error "Age cannot be negative"
    | n > 150   = Error "Age seems unrealistic"
    | otherwise = Success n
```

## Common Pitfalls

### 1. Partial Functions

```haskell
-- Dangerous: partial function
head'' :: [a] -> a
head'' (x:_) = x  -- Crashes on empty list

-- Safe: total function with Maybe
safeHead' :: [a] -> Maybe a
safeHead' []    = Nothing
safeHead' (x:_) = Just x
```

### 2. Performance Considerations

```haskell
-- Inefficient: multiple traversals
length' :: [a] -> Int
length' xs = if null xs then 0 else 1 + length' (tail xs)

-- Efficient: single traversal with accumulator
length'' :: [a] -> Int
length'' = go 0
  where
    go !n []     = n
    go !n (_:xs) = go (n + 1) xs
```

### 3. n+k Patterns (Deprecated)

```haskell
-- Avoid n+k patterns (no longer supported)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)  -- Use explicit subtraction

-- Better: guard-based approach
factorial' :: Integer -> Integer
factorial' n
    | n < 0     = error "Factorial undefined for negative numbers"
    | n == 0    = 1
    | otherwise = n * factorial' (n-1)
```

## Advanced Examples

### Recursive Pattern Matching

```haskell
-- Sum a list using pattern matching and recursion
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Tree traversal with pattern matching
data Tree a = Empty | Node a (Tree a) (Tree a)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) =
    1 + max (treeDepth left) (treeDepth right)
```

### Pattern Matching with Maybe

```haskell
-- Combining Maybe values
combine :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
combine Nothing _ _  = Nothing
combine _ Nothing _  = Nothing
combine (Just x) (Just y) f = Just (f x y)

-- Safe division with pattern matching
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)
```

## Conclusion

Pattern matching is a cornerstone of Haskell programming that enables:
- Clear and concise code
- Type-safe data destructuring
- Elegant handling of complex data structures
- Powerful conditional logic

Master pattern matching early in your Haskell journey, as it's essential for writing idiomatic and maintainable code. Remember to:
- Ensure pattern coverage is complete
- Order patterns from specific to general
- Use wildcards judiciously
- Consider performance implications
- Leverage pattern matching for safer error handling

With practice, pattern matching will become one of your most valuable tools for writing elegant and reliable Haskell code. 