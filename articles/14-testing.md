# Testing in Haskell

Testing is crucial for writing reliable software, and Haskell's pure functions make testing particularly straightforward and powerful. Let's explore the various testing approaches available in Haskell!

## Why Testing is Easy in Haskell

Haskell's features make it particularly suitable for testing:
- Pure functions always give the same output for the same input
- Strong type system catches many errors at compile time
- Property-based testing works well with pure functions
- QuickCheck can generate test cases automatically

## Unit Testing with HUnit

HUnit is Haskell's standard unit testing framework:

```haskell
import Test.HUnit

-- Function to test
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Test cases
tests :: Test
tests = TestList
    [ TestCase $ assertEqual "factorial 0" 1 (factorial 0)
    , TestCase $ assertEqual "factorial 3" 6 (factorial 3)
    , TestCase $ assertEqual "factorial 5" 120 (factorial 5)
    ]

-- Run tests
main :: IO Counts
main = runTestTT tests
```

## Property-Based Testing with QuickCheck

QuickCheck generates test cases automatically based on properties:

```haskell
import Test.QuickCheck

-- Properties
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

prop_lengthNonNegative :: [Int] -> Bool
prop_lengthNonNegative xs = length xs >= 0

-- Run QuickCheck
main :: IO ()
main = do
    quickCheck prop_reverseReverse
    quickCheck prop_lengthNonNegative
```

### Custom Generators

```haskell
-- Generate custom test data
data User = User String Int deriving Show

instance Arbitrary User where
    arbitrary = do
        name <- elements ["Alice", "Bob", "Charlie"]
        age <- choose (18, 100)
        return $ User name age

-- Property using custom type
prop_userAgeValid :: User -> Bool
prop_userAgeValid (User _ age) = age >= 18
```

## Integration Testing

Testing functions that work together:

```haskell
-- System under test
data ShoppingCart = Cart [Item] deriving Show
data Item = Item String Double deriving Show

addItem :: Item -> ShoppingCart -> ShoppingCart
addItem item (Cart items) = Cart (item : items)

calculateTotal :: ShoppingCart -> Double
calculateTotal (Cart items) = sum [price | Item _ price <- items]

-- Integration tests
tests :: Test
tests = TestList
    [ TestCase $ do
        let cart = Cart []
            item1 = Item "Book" 10.0
            item2 = Item "Pen" 2.0
            finalCart = addItem item2 (addItem item1 cart)
        assertEqual "total price" 12.0 (calculateTotal finalCart)
    ]
```

## Testing IO Code

Testing functions with side effects:

```haskell
-- Function with IO
readAndProcess :: FilePath -> IO String
readAndProcess path = do
    contents <- readFile path
    return $ processContents contents
  where
    processContents = unlines . reverse . lines

-- Test using temporary files
import System.IO.Temp
import System.IO

testReadAndProcess :: Test
testReadAndProcess = TestCase $ do
    -- Create temporary file
    withSystemTempFile "test.txt" $ \path handle -> do
        -- Write test data
        hPutStr handle "line 1\nline 2\nline 3"
        hClose handle
        
        -- Run function
        result <- readAndProcess path
        
        -- Check result
        assertEqual "reversed lines"
            "line 3\nline 2\nline 1\n"
            result
```

## Test Organization

### Using Test Suites

```haskell
-- Test suite organization
tests :: Test
tests = TestList
    [ TestLabel "Unit Tests" unitTests
    , TestLabel "Integration Tests" integrationTests
    , TestLabel "Property Tests" propertyTests
    ]
  where
    unitTests = TestList
        [ TestCase $ assertEqual "simple case" 2 (1 + 1)
        , TestCase $ assertBool "boolean case" True
        ]
    
    integrationTests = TestList
        [ testReadAndProcess
        , testDatabaseOperations
        ]
    
    propertyTests = TestList
        [ TestCase $ quickCheck prop_reverseReverse
        , TestCase $ quickCheck prop_lengthNonNegative
        ]
```

## Testing with Cabal

### cabal.project Configuration

```yaml
test-suite my-test-suite
  type:                exitcode-stdio-1.0
  main-is:            Test.hs
  build-depends:      base
                    , HUnit
                    , QuickCheck
                    , my-project
  hs-source-dirs:     test
  default-language:   Haskell2010
```

### Directory Structure

```
my-project/
├── src/
│   └── MyLib.hs
├── test/
│   ├── Test.hs
│   ├── UnitTests.hs
│   └── PropertyTests.hs
└── my-project.cabal
```

## Best Practices

### 1. Test Organization

```haskell
-- Group related tests
numberTests :: Test
numberTests = TestList
    [ TestLabel "Addition" $ TestList
        [ TestCase $ assertEqual "positive" 4 (2 + 2)
        , TestCase $ assertEqual "negative" (-2) ((-1) + (-1))
        ]
    , TestLabel "Multiplication" $ TestList
        [ TestCase $ assertEqual "positive" 4 (2 * 2)
        , TestCase $ assertEqual "negative" 2 ((-1) * (-2))
        ]
    ]
```

### 2. Helper Functions

```haskell
-- Create helpers for common assertions
assertInRange :: (Ord a, Show a) => String -> (a, a) -> a -> Assertion
assertInRange msg (min, max) actual =
    assertBool msg $ actual >= min && actual <= max

-- Using helper
testAge :: Test
testAge = TestCase $
    assertInRange "age in valid range" (0, 150) calculatedAge
```

### 3. Test Data Generation

```haskell
-- Generate test data
data TestData = TestData
    { testInput :: String
    , expectedOutput :: String
    }

testCases :: [TestData]
testCases =
    [ TestData "input1" "expected1"
    , TestData "input2" "expected2"
    ]

-- Use test data
tests :: Test
tests = TestList
    [ TestCase $ assertEqual msg expected (process input)
    | TestData input expected <- testCases
    , let msg = "Processing " ++ input
    ]
```

## Advanced Testing Techniques

### 1. Golden Testing

```haskell
import Test.Framework.Providers.Golden

-- Compare output with stored "golden" file
goldenTest :: IO ()
goldenTest = do
    actual <- generateOutput
    golden <- readFile "test/golden/output.txt"
    assertEqual "matches golden file" golden actual
```

### 2. Performance Testing

```haskell
import Criterion.Main

-- Benchmark performance
main :: IO ()
main = defaultMain
    [ bench "sort small list" $ 
        whnf sort [5,4,3,2,1]
    , bench "sort large list" $ 
        whnf sort [1000,999..1]
    ]
```

## Next Steps

After mastering testing, you can:
- Set up continuous integration
- Write property-based tests for complex systems
- Implement automated test suites
- Use advanced testing frameworks
- Practice test-driven development

Remember:
- Write tests for all new functionality
- Use property-based testing where appropriate
- Keep tests simple and focused
- Maintain your test suite
- Use testing to catch bugs early
</rewritten_file> 