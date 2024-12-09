# Lists in Haskell

Lists are one of the most important features in Haskell. They're simple but powerful, and you'll use them all the time. Let's learn how they work!

## Creating Lists

Lists in Haskell are written with square brackets:

```haskell
numbers = [1, 2, 3, 4, 5]
names = ["Alice", "Bob", "Charlie"]
empty = []  -- an empty list
```

All items in a list must be the same type. This works:
```haskell
goodList = [1, 2, 3]  -- all Integers
```

This doesn't:
```haskell
badList = [1, "hello", True]  -- ERROR: can't mix types
```

## List Ranges

Haskell has a nice shortcut for creating number lists:

```haskell
-- Different ways to create ranges
oneToTen = [1..10]          -- [1,2,3,4,5,6,7,8,9,10]
letters = ['a'..'e']        -- ['a','b','c','d','e']
evens = [2,4..10]          -- [2,4,6,8,10]
countdown = [10,9..1]       -- [10,9,8,7,6,5,4,3,2,1]
```

## Basic List Operations

### Adding Elements

The `:` operator (called "cons") adds an element to the front of a list:

```haskell
numbers = 1 : [2, 3, 4]    -- [1,2,3,4]
newList = 5 : []           -- [5]
```

### Combining Lists

Use `++` to combine lists:

```haskell
first = [1, 2, 3]
second = [4, 5, 6]
combined = first ++ second  -- [1,2,3,4,5,6]
```

### Getting Elements

Use `!!` to get an element by position (starting from 0):

```haskell
numbers = [10, 20, 30, 40]
first = numbers !! 0    -- 10
second = numbers !! 1   -- 20
```

## Common List Functions

Here are the most useful functions for working with lists:

```haskell
numbers = [1, 2, 3, 4, 5]

-- Getting list information
length numbers        -- 5
head numbers         -- 1 (first element)
tail numbers         -- [2,3,4,5] (everything but first)
last numbers         -- 5 (last element)
init numbers         -- [1,2,3,4] (everything but last)

-- Checking lists
null []              -- True (is it empty?)
null numbers         -- False
elem 3 numbers       -- True (is 3 in the list?)
```

## List Processing Functions

These functions help you work with lists:

```haskell
numbers = [1, 2, 3, 4, 5]

-- Transforming lists
reverse numbers      -- [5,4,3,2,1]
take 3 numbers      -- [1,2,3]
drop 2 numbers      -- [3,4,5]
maximum numbers     -- 5
minimum numbers     -- 1
sum numbers         -- 15
product numbers     -- 120
```

## List Comprehensions

List comprehensions are a powerful way to create lists:

```haskell
-- Double each number
doubles = [x * 2 | x <- [1..5]]
-- Result: [2,4,6,8,10]

-- Only even numbers
evens = [x | x <- [1..10], even x]
-- Result: [2,4,6,8,10]

-- Combine lists
pairs = [(x,y) | x <- [1,2], y <- ['a','b']]
-- Result: [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

## Practice Exercises

Try these exercises:

```haskell
-- 1. Create a list of squares from 1 to 5
squares = [x * x | x <- [1..5]]  -- [1,4,9,16,25]

-- 2. Filter out odd numbers
noOdds = [x | x <- [1..10], x `mod` 2 == 0]

-- 3. Create a function that doubles all elements
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x * 2 | x <- xs]
```

## Common Patterns

Here are some common ways to use lists:

```haskell
-- Building a list one element at a time
buildUp = 1 : 2 : 3 : []  -- [1,2,3]

-- Taking parts of lists
first3 = take 3 [1..10]   -- [1,2,3]
rest = drop 1 [1..5]      -- [2,3,4,5]

-- Checking conditions
hasEven = any even [1,3,5,6,7]  -- True
allSmall = all (<10) [1,2,3,4]  -- True
```

## Next Steps

Now that you understand lists, you can:
- Learn about list recursion
- Explore higher-order functions like `map` and `filter`
- Work with infinite lists (yes, Haskell can handle those!)

Remember: Lists are your friends in Haskell. Practice using them, and you'll find they make many programming tasks much easier! 