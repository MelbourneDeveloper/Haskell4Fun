# Recursion in Haskell

Recursion is a cornerstone of functional programming. In Haskell, it's how we handle repetitive tasks without using traditional loops. Don't worry - we'll make it clear and practical!

## What is Recursion?

Recursion is when a function calls itself to solve a problem. Think of it as breaking down a big problem into smaller, similar problems.

## Basic Recursion Patterns

### Simple Recursion

Let's start with a classic example - calculating factorial:

```haskell
factorial :: Integer -> Integer
factorial 0 = 1                        -- Base case
factorial n = n * factorial (n - 1)    -- Recursive case

-- Example usage:
-- factorial 5 = 5 * 4 * 3 * 2 * 1 = 120
```

### List Recursion

Processing lists recursively is very common in Haskell:

```haskell
-- Sum all elements in a list
sum' :: Num a => [a] -> a
sum' [] = 0                -- Base case: empty list
sum' (x:xs) = x + sum' xs -- Recursive case: head + sum of tail

-- Length of a list
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs
```

## Tail Recursion

Tail recursion is a special form of recursion that's more efficient:

```haskell
-- Regular recursion (builds up stack)
factorial1 :: Integer -> Integer
factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)

-- Tail recursive version (doesn't build stack)
factorial2 :: Integer -> Integer
factorial2 n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)
```

## Common Recursive Patterns

### Processing Lists

```haskell
-- Map implemented recursively
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Filter implemented recursively
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
```

### Binary Trees

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

-- Count nodes in a tree
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- Tree depth
treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)
```

## Mutual Recursion

Sometimes functions call each other recursively:

```haskell
-- Check if a number is even or odd
isEven :: Integer -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Integer -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)
```

## Advanced Recursion Techniques

### Accumulator Pattern

Use an accumulator to build up results efficiently:

```haskell
-- Reverse a list using an accumulator
reverse' :: [a] -> [a]
reverse' xs = go xs []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)
```

### Multiple Accumulators

Sometimes you need multiple accumulators:

```haskell
-- Split a list into evens and odds
partition :: [a] -> ([a], [a])
partition = go [] []
  where
    go evens odds [] = (reverse evens, reverse odds)
    go evens odds (x:xs) = go odds (x:evens) xs
```

## Common Pitfalls and Solutions

### Stack Overflow

Problem:
```haskell
-- This might stack overflow
sum' (x:xs) = x + sum' xs
```

Solution (use tail recursion):
```haskell
sum' xs = go 0 xs
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs
```

### Infinite Recursion

Problem:
```haskell
-- This never terminates
factorial n = n * factorial n
```

Solution (ensure progress towards base case):
```haskell
factorial n = n * factorial (n - 1)  -- Gets closer to base case
factorial 0 = 1                      -- Base case stops recursion
```

## Practice Exercises

1. Implement quicksort recursively:
```haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger
```

2. Write a function to flatten a tree:
```haskell
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node x left right) = flatten left ++ [x] ++ flatten right
```

## Tips for Writing Recursive Functions

1. **Always start with the base case**
   - What's the simplest input?
   - What should the function return for that input?

2. **Make the problem smaller**
   - Each recursive call should work on a smaller version of the problem
   - Ensure you're getting closer to the base case

3. **Think about efficiency**
   - Use tail recursion for long lists
   - Consider using accumulators
   - Watch out for unnecessary concatenations

## When to Use Recursion

Recursion is great for:
- Processing nested structures (trees, lists)
- Problems that have a natural recursive structure
- Implementing mathematical functions
- Creating elegant, declarative solutions

But consider alternatives when:
- The problem is naturally iterative
- You need maximum performance
- The solution would be clearer with higher-order functions

## Next Steps

Now that you understand recursion, you can:
- Study more advanced recursive patterns
- Learn about corecursion and infinite structures
- Explore how recursion relates to mathematical induction
- Practice implementing classic algorithms recursively

Remember: Recursion is a powerful tool, but it's not always the best solution. Use it when it makes your code clearer and more maintainable! 