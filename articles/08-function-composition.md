# Function Composition

## Table of Contents

- [Function Composition](#function-composition)
  - [Table of Contents](#table-of-contents)
  - [Additional Resources](#additional-resources)
  - [Introduction](#introduction)
  - [Basic Function Composition](#basic-function-composition)
  - [The Composition Operator](#the-composition-operator)
  - [Real-World Examples](#real-world-examples)
    - [Text Processing](#text-processing)
    - [Number Processing](#number-processing)
  - [Common Patterns](#common-patterns)
    - [Data Transformation Pipelines](#data-transformation-pipelines)
    - [Optional Value Handling](#optional-value-handling)
  - [Best Practices](#best-practices)
    - [1. Keep Functions Simple and Focused](#1-keep-functions-simple-and-focused)
    - [2. Use Type Signatures](#2-use-type-signatures)
  - [Advanced Composition](#advanced-composition)
    - [Working with Different Types](#working-with-different-types)
  - [Debugging Tips](#debugging-tips)
  - [Common Mistakes](#common-mistakes)
    - [1. Wrong Composition Order](#1-wrong-composition-order)
    - [2. Forgetting to Handle Edge Cases](#2-forgetting-to-handle-edge-cases)
  - [When to Use Composition](#when-to-use-composition)
  - [Conclusion](#conclusion)

## Additional Resources

- [Haskell.org - Function Composition](https://wiki.haskell.org/Function_composition)
- [GHC Documentation - Base: Data.Function](https://hackage.haskell.org/package/base/docs/Data-Function.html)
- [Learn You a Haskell - Higher Order Functions](http://learnyouahaskell.com/higher-order-functions)

## Introduction

Imagine you're building a sandwich. You start with bread, add lettuce, then tomatoes, then cheese, and finally more bread. Each step transforms your sandwich into something new. Function composition in Haskell works the same way - it's about taking simple functions and combining them to create more complex operations.

In programming terms, function composition means taking the output of one function and immediately using it as the input for another function. This creates a new function that does both operations in sequence.

## Basic Function Composition

Let's start with something simple that everyone can understand - working with numbers:

```haskell
-- First, let's create two simple functions
double x = x * 2
addOne x = x + 1

-- We can use these functions separately:
result1 = double (addOne 5)  -- This gives us 12

-- Or we can compose them into a new function:
doubleAfterAddOne = double . addOne
result2 = doubleAfterAddOne 5  -- This also gives us 12
```

Let's break down what's happening here:
1. When we write `double (addOne 5)`:
   - First, `addOne 5` is calculated, giving us 6
   - Then, `double` is applied to 6, giving us 12

2. When we write `double . addOne`:
   - We create a new function that automatically does both operations
   - The dot (`.`) tells Haskell to "pipe" the result of `addOne` into `double`

This is like saying "First add one, then double the result" - but we only have to say it once, when we create the composed function.

## The Composition Operator

The dot operator (`.`) is the star of the show in function composition. Here's how it really works:

```haskell
-- These three ways of writing the same thing are equivalent:
method1 x = f (g x)
method2 x = (f . g) x
method3 = f . g

-- Real-world example: Making text exciting
makeExciting = (++ "!") . toUpper

putExcited = putStrLn . makeExciting
-- putExcited "hello" prints "HELLO!"

-- Let's break this down:
-- 1. toUpper "hello" -> "HELLO"
-- 2. (++ "!") "HELLO" -> "HELLO!"
-- 3. putStrLn "HELLO!" -> prints to screen
```

Why is this useful? Because:
1. It's more readable - we can see the transformation steps clearly
2. It's reusable - we can create new functions by mixing and matching existing ones
3. It's maintainable - we can modify one step without touching the others

## Real-World Examples

### Text Processing

Here's a real-world example of processing user input:

```haskell
-- Let's create a function to clean up user input
cleanUserInput :: String -> String
cleanUserInput = removeExtraSpaces . removePunctuation . toLower
  where
    -- Helper functions:
    removeExtraSpaces = unwords . words  -- Replace multiple spaces with single space
    removePunctuation = filter (`notElem` ".,!?")  -- Remove common punctuation

-- Let's see it in action:
-- cleanUserInput "Hello,    World!!!"
-- Step 1 (toLower): "hello,    world!!!"
-- Step 2 (removePunctuation): "hello    world"
-- Step 3 (removeExtraSpaces): "hello world"

-- Now let's create a function to format names
formatName :: String -> String
formatName = unwords . map capitalize . words . toLower
  where
    capitalize "" = ""
    capitalize (x:xs) = toUpper x : xs

-- Example:
-- formatName "jOHN     sMITH"
-- Step 1 (toLower): "john     smith"
-- Step 2 (words): ["john", "smith"]
-- Step 3 (map capitalize): ["John", "Smith"]
-- Step 4 (unwords): "John Smith"
```

Each step in these functions has a clear purpose:
1. `toLower` standardizes the input
2. `words` splits on whitespace
3. `map capitalize` transforms each word
4. `unwords` rebuilds the string

### Number Processing

Let's look at some numerical processing examples:

```haskell
-- Calculate the average of even numbers in a list
averageEvens :: [Int] -> Double
averageEvens = average . filter even
  where
    average nums = fromIntegral (sum nums) / fromIntegral (length nums)

-- Let's break down what happens with [1,2,3,4,5,6]:
-- 1. filter even [1,2,3,4,5,6] -> [2,4,6]
-- 2. sum [2,4,6] -> 12
-- 3. length [2,4,6] -> 3
-- 4. fromIntegral 12 / fromIntegral 3 -> 4.0

-- Process a list of scores
processScores :: [Int] -> String
processScores = showPercentage . calculateAverage . validateScores
  where
    validateScores = filter (\x -> x >= 0 && x <= 100)
    calculateAverage nums = sum nums `div` length nums
    showPercentage n = show n ++ "%"

-- Example:
-- processScores [85, 90, 95, 105, -10]
-- Step 1 (validateScores): [85, 90, 95]
-- Step 2 (calculateAverage): 90
-- Step 3 (showPercentage): "90%"
```

## Common Patterns

Here are some patterns you'll see often in Haskell code:

### Data Transformation Pipelines

```haskell
-- Process user registration data
processRegistration :: UserInput -> Either String User
processRegistration = validateUser . sanitizeInput . normalizeFields
  where
    normalizeFields input = input { 
        username = toLower (username input),
        email = toLower (email input)
    }
    
    sanitizeInput input = input {
        username = filter isAlphaNum (username input),
        email = filter (\c -> isAlphaNum c || c `elem` "@.") (email input)
    }
    
    validateUser input
        | length (username input) < 3 = Left "Username too short"
        | not (isValid (email input)) = Left "Invalid email"
        | otherwise = Right input

-- Example:
-- processRegistration (UserInput "John123!" "JOHN@email.com")
-- Step 1 (normalizeFields): UserInput "john123!" "john@email.com"
-- Step 2 (sanitizeInput): UserInput "john123" "john@email.com"
-- Step 3 (validateUser): Right (User "john123" "john@email.com")
```

### Optional Value Handling

```haskell
-- Find a user's premium status through several lookups
getPremiumStatus :: String -> Maybe Bool
getPremiumStatus = fmap isPremium . getUser . lookupSession
  where
    lookupSession :: String -> Maybe String
    lookupSession = -- looks up user session
    
    getUser :: String -> Maybe User
    getUser = -- gets user from session
    
    isPremium :: User -> Bool
    isPremium = -- checks premium status

-- Example:
-- getPremiumStatus "session123"
-- Step 1 (lookupSession): Just "user456"
-- Step 2 (getUser): Just (User "john" "premium")
-- Step 3 (fmap isPremium): Just True
```

## Best Practices

### 1. Keep Functions Simple and Focused

```haskell
-- Bad: One complex function
processData input = 
    let cleaned = filter isValid input
        normalized = map normalize cleaned
        validated = all checkRules normalized
    in if validated 
       then Just normalized
       else Nothing

-- Good: Composed simple functions
processData = validateAll . normalizeAll . cleanInput
  where
    cleanInput = filter isValid
    normalizeAll = map normalize
    validateAll xs = if all checkRules xs 
                    then Just xs 
                    else Nothing
```

The second version is better because:
1. Each function has one clear purpose
2. Functions are reusable
3. The pipeline is easy to modify
4. The code reads like a description of the process

### 2. Use Type Signatures

```haskell
-- Bad: Missing type signatures
process = format . validate . clean

-- Good: Clear type signatures
clean :: String -> String
clean = filter isAlphaNum

validate :: String -> Maybe String
validate s = if not (null s) then Just s else Nothing

format :: Maybe String -> String
format = maybe "Invalid" id

process :: String -> String
process = format . validate . clean
```

Type signatures help by:
1. Making the code self-documenting
2. Catching errors early
3. Making refactoring easier

## Advanced Composition

### Working with Different Types

```haskell
-- Converting between types in a composition
getData :: String -> Maybe Int
getData = readMaybe . filter isDigit . toLower

-- Let's break it down:
-- Input: "Age: 25"
-- Step 1 (toLower): "age: 25"
-- Step 2 (filter isDigit): "25"
-- Step 3 (readMaybe): Just 25

-- Working with lists and Maybe
findUserName :: Database -> Int -> Maybe String
findUserName db = fmap name . lookupUser db
  where
    lookupUser :: Database -> Int -> Maybe User
    name :: User -> String

-- Example:
-- findUserName db 123
-- Step 1 (lookupUser db): Just (User "John" 30)
-- Step 2 (fmap name): Just "John"
```

## Debugging Tips

When working with composed functions, debugging can be tricky. Here's how to do it effectively:

```haskell
-- Add debug traces to your composition
import Debug.Trace

debugTrace :: Show a => String -> a -> a
debugTrace label x = trace (label ++ ": " ++ show x) x

-- Use it in your composition
processWithDebug :: String -> String
processWithDebug = format 
                 . debugTrace "after validate"
                 . validate
                 . debugTrace "after clean"
                 . clean

-- Example output:
-- "after clean: "hello123""
-- "after validate: Just "hello123""
-- "hello123"
```

## Common Mistakes

### 1. Wrong Composition Order

```haskell
-- Wrong: Tries to use length before converting to string
wrong = length . show
-- This fails because 'show' produces a String, but 'length' expects a list

-- Right: Convert to string first, then get length
right = show . length
-- This works because 'length' produces a number, which 'show' can convert

-- Example:
-- Input: [1,2,3]
-- wrong: Type error!
-- right: "3"
```

### 2. Forgetting to Handle Edge Cases

```haskell
-- Wrong: Doesn't handle empty lists
wrong = head . sort
-- This will crash on empty lists

-- Right: Handles empty lists safely
right = listToMaybe . sort
-- Returns Nothing for empty lists

-- Even better: Make it explicit in the type
safeFirst :: Ord a => [a] -> Maybe a
safeFirst = listToMaybe . sort

-- Example:
-- wrong [] -- Crashes!
-- right [] -- Returns Nothing
-- right [3,1,2] -- Returns Just 1
```

## When to Use Composition

Use function composition when:
1. You have a clear sequence of transformations
2. Each step has a single responsibility
3. The output types match the input types of the next function
4. The composition makes the code more readable

Don't use composition when:
1. The steps are complex or have multiple branches
2. You need to handle errors differently at each step
3. The composition would make the code harder to understand

## Conclusion

Function composition is a powerful tool that helps you:
- Write cleaner, more maintainable code
- Break complex problems into simple pieces
- Create reusable function pipelines
- Express data transformations clearly

Key takeaways:
1. Think of composition as a pipeline of transformations
2. Keep individual functions simple and focused
3. Use type signatures to catch errors early
4. Debug composed functions by adding traces
5. Consider readability when deciding whether to use composition

Remember: The goal of function composition is to make your code more readable and maintainable. If a composition makes your code harder to understand, it might not be the right tool for that particular situation.

Happy composing!