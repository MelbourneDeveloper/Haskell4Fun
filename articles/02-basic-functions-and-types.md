# Basic Functions and Types in Haskell

Let's explore Haskell's type system and function definitions, focusing on their mathematical foundations and practical applications.

## Functions: Pure by Default

In Haskell, functions are:
1. Pure (referentially transparent)
2. First-class citizens
3. Curried by default

Here's a simple function with its type signature:

```haskell
double :: Num a => a -> a
double x = x * 2
```

Let's analyze this:
- `double` is the function name
- `Num a =>` is a type constraint requiring `a` to be a numeric type
- `a -> a` means it takes and returns the same type
- The implementation shows referential transparency

## Function Application

Function application in Haskell is left-associative and has the highest precedence:

```haskell
result = double 5      -- 10
nested = double (double 3)  -- 12
composed = (double . double) 3  -- Same as above
```

## Currying and Partial Application

All Haskell functions are automatically curried:

```haskell
add :: Num a => a -> a -> a
add x y = x + y

-- These are equivalent:
add3 = add 3      -- Partial application
addThree y = add 3 y  -- Explicit parameter

-- Using the partially applied function
result = add3 7   -- 10
```

## Fundamental Types

Haskell's type system is rich and expressive:

```haskell
-- Numeric types
int :: Int            -- Fixed-precision integer
integer :: Integer    -- Arbitrary-precision integer
float :: Float        -- Single-precision floating point
double :: Double      -- Double-precision floating point
rational :: Rational  -- Exact rational numbers

-- Textual types
char :: Char          -- Unicode character
text :: Text          -- Efficient text type (from Data.Text)
string :: String      -- [Char] - list of characters

-- Boolean
bool :: Bool          -- True | False

-- Unit type
unit :: ()            -- Only one value: ()
```

## Type Classes and Polymorphism

Haskell uses type classes for ad-hoc polymorphism:

```haskell
-- Polymorphic equality check
equal :: Eq a => a -> a -> Bool
equal x y = x == y

-- Polymorphic comparison
compare :: Ord a => a -> a -> Ordering
compare x y = x `compare` y

-- Polymorphic show function
showValue :: Show a => a -> String
showValue x = show x
```

## Type Variables and Constraints

Type variables enable parametric polymorphism:

```haskell
-- Identity function: works with any type
id :: a -> a
id x = x

-- Function composition
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Multiple constraints
sortAndShow :: (Ord a, Show a) => [a] -> String
sortAndShow xs = show (sort xs)
```

## Practice Exercises

Try implementing these polymorphic functions:

```haskell
-- 1. A function that works with any numeric type
square :: Num a => a -> a
square x = x * x

-- 2. A function that works with any orderable type
maximum3 :: Ord a => a -> a -> a -> a
maximum3 x y z = maximum [x, y, z]

-- 3. A function that combines Show and Num constraints
showDouble :: (Show a, Num a) => a -> String
showDouble x = show (x * 2)
```

## Common Pitfalls and Solutions

1. **Type Class Constraints**
```haskell
-- Wrong: Missing Num constraint
add x y = x + y

-- Correct: With constraint
add :: Num a => a -> a -> a
add x y = x + y
```

2. **Function Composition and Application**
```haskell
-- Wrong: Precedence issue
double x + 2   -- Equivalent to: (double x) + 2

-- Correct: Using composition
(2+) . double  -- Function that doubles then adds 2
```

3. **Point-free Style**
```haskell
-- With parameters
addAndDouble x y = double (x + y)

-- Point-free version
addAndDouble = double . uncurry (+)
```

## Next Steps

With these fundamentals, you're ready to explore:
- Pattern matching and guards
- Algebraic data types
- Higher-order functions
- Type class implementations
- Monadic computations

Remember: Haskell's type system is your friend - it helps catch errors at compile time and makes your code more reliable. 