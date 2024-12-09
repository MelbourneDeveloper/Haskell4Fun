# Introduction to Haskell

Welcome to Haskell! This guide will introduce you to one of the most powerful and elegant programming languages. We'll focus on practical understanding while maintaining theoretical accuracy.

## What is Haskell?

Haskell is a purely functional programming language that offers unique characteristics:

- It's **purely functional** - functions are first-class citizens and have no side effects
- It's **statically typed** - with powerful type inference and an expressive type system
- It's **non-strict** (lazy) - expressions are only evaluated when their results are needed
- It's **pure** - functions always return the same output for the same input

## Why Learn Haskell?

1. **Mathematical Reasoning**: Haskell's pure functions and strong type system enable equational reasoning
2. **Type Safety**: The advanced type system prevents entire classes of runtime errors
3. **Declarative Style**: Express what you want to compute, not how to compute it
4. **Performance**: Lazy evaluation and pure functions enable powerful optimizations

## Your First Haskell Program

Let's write the classic "Hello, World!" program:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

Let's understand each part:

- `main` is the program's entry point
- `::` is the type signature operator, reading as "has type"
- `IO ()` indicates this is an I/O action returning unit `()`
- `putStrLn` is an I/O action that prints a string with a newline

## Basic Values and Types

Haskell is strongly typed. Here are some fundamental examples:

```haskell
-- Numbers
x :: Integer    -- Arbitrary-precision integer
x = 42          

y :: Double     -- Double-precision floating point
y = 3.14        

-- Text
name :: String  -- A list of characters
name = "Alice"  

-- Booleans
isTrue :: Bool  -- Boolean type
isTrue = True   

-- Characters
ch :: Char      -- Unicode character
ch = 'a'
```

## Running Haskell Code

To work with Haskell:

1. Install GHC (Glasgow Haskell Compiler)
2. Save your code with a `.hs` extension
3. Use:
   - `ghc file.hs` to compile
   - `runghc file.hs` to run without explicit compilation
   - `ghci` for interactive development

## What's Next?

In the following articles, we'll explore:

- Functions and Type Signatures
- Pattern Matching and Guards
- Algebraic Data Types
- Higher-Order Functions
- Typeclasses and Polymorphism

Remember: Haskell's approach might be different from other languages, but its principles lead to more reliable and maintainable code.

## Practice Exercise

Here's a more idiomatic Haskell example:

```haskell
-- A pure function that adds two numbers
add :: Num a => a -> a -> a
add x y = x + y

-- A function that doubles a number using partial application
double :: Num a => a -> a
double = add <*> id  -- Point-free style, using applicative

main :: IO ()
main = do
    putStrLn "Demonstrating pure functions:"
    print $ double 5        -- Will print 10
    print $ add 7 3        -- Will print 10
    print $ map double [1,2,3]  -- Will print [2,4,6]
```

This example introduces:
- Polymorphic type signatures with constraints
- Pure functions
- Partial application
- Point-free style
- List operations 