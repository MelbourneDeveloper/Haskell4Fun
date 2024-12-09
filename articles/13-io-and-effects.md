# IO and Effects in Haskell

Understanding how to handle input/output and side effects is crucial for writing real-world Haskell programs. Let's learn how Haskell manages these while maintaining its pure functional nature!

## Pure Functions vs. IO

In Haskell, we distinguish between:
- Pure functions (no side effects)
- IO actions (can have side effects)

```haskell
-- Pure function (same input always gives same output)
double :: Integer -> Integer
double x = x * 2

-- IO action (can interact with the outside world)
getName :: IO String
getName = do
    putStr "Enter your name: "
    getLine
```

## Basic IO Operations

### Reading and Writing

```haskell
-- Print to console
main :: IO ()
main = do
    putStrLn "Hello!"              -- Print with newline
    putStr "No newline"            -- Print without newline
    print 42                       -- Print any showable value

-- Reading input
main :: IO ()
main = do
    line <- getLine               -- Read a line
    char <- getChar              -- Read a single character
    contents <- readFile "file.txt"  -- Read entire file
```

### Working with Files

```haskell
-- File operations
main :: IO ()
main = do
    -- Writing to a file
    writeFile "output.txt" "Hello, World!"
    appendFile "log.txt" "New log entry\n"
    
    -- Reading from a file
    contents <- readFile "input.txt"
    putStrLn contents
    
    -- Working with handles for more control
    handle <- openFile "data.txt" ReadMode
    data <- hGetContents handle
    hClose handle
```

## The do Notation

`do` notation makes IO code look more imperative:

```haskell
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn "What's your age?"
    ageStr <- getLine
    let age = read ageStr :: Int
    putStrLn $ "Hello, " ++ name ++ "! "
             ++ "You'll be " ++ show (age + 1)
             ++ " next year."
```

## Combining Pure and IO Code

### Pure Functions in IO

```haskell
-- Pure function
calculateArea :: Double -> Double -> Double
calculateArea width height = width * height

-- Using pure function in IO
main :: IO ()
main = do
    putStrLn "Enter width:"
    width <- readLn
    putStrLn "Enter height:"
    height <- readLn
    let area = calculateArea width height
    putStrLn $ "The area is: " ++ show area
```

### Handling Pure Computations

```haskell
processData :: String -> String
processData = map toUpper

main :: IO ()
main = do
    input <- getLine
    let processed = processData input  -- Pure computation
    putStrLn processed                 -- IO action
```

## Error Handling in IO

### Using Maybe and Either

```haskell
-- Safe file reading
readFileSafely :: FilePath -> IO (Either String String)
readFileSafely path = do
    exists <- doesFileExist path
    if exists
        then Right <$> readFile path
        else return $ Left "File does not exist"

-- Using it
main :: IO ()
main = do
    result <- readFileSafely "data.txt"
    case result of
        Right contents -> putStrLn $ "File contents: " ++ contents
        Left error -> putStrLn $ "Error: " ++ error
```

### Exception Handling

```haskell
import Control.Exception

-- Catching specific exceptions
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
    result <- try (readFile path) :: IO (Either IOException String)
    return $ case result of
        Right contents -> Right contents
        Left ex -> Left (show ex)

-- Using bracket for resource management
withFile' :: FilePath -> (Handle -> IO a) -> IO a
withFile' path action = bracket
    (openFile path ReadMode)  -- acquire resource
    hClose                    -- release resource
    action                    -- use resource
```

## Working with Command Line Arguments

```haskell
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> processFiles input output
        _ -> do
            putStrLn "Usage: program input.txt output.txt"
            exitFailure

processFiles :: FilePath -> FilePath -> IO ()
processFiles input output = do
    contents <- readFile input
    writeFile output (processData contents)
```

## Real-World Examples

### Simple File Processing Program

```haskell
-- Count words in a file
countWords :: FilePath -> IO ()
countWords path = do
    contents <- readFile path
    let wordCount = length (words contents)
    putStrLn $ "The file contains " ++ show wordCount ++ " words."

-- Process multiple files
processFiles :: [FilePath] -> IO ()
processFiles = mapM_ $ \path -> do
    putStrLn $ "\nProcessing: " ++ path
    countWords path
```

### Interactive Program

```haskell
data Command = Add String | View | Quit

-- Simple todo list program
todoProgram :: IO ()
todoProgram = do
    putStrLn "Todo List Program"
    loop []
  where
    loop todos = do
        putStrLn "\nCommands: add <task>, view, quit"
        putStr "> "
        cmd <- parseCommand <$> getLine
        case cmd of
            Add task -> do
                putStrLn "Added task"
                loop (task:todos)
            View -> do
                putStrLn "Tasks:"
                mapM_ (\(i,t) -> putStrLn $ show i ++ ". " ++ t)
                      (zip [1..] todos)
                loop todos
            Quit -> putStrLn "Goodbye!"
```

## Best Practices

1. **Keep IO at the Edges**
   
```haskell
-- Good: Separate IO from pure computation
main :: IO ()
main = do
    input <- getLine
    let result = pureComputation input  -- Pure core
    putStrLn result

-- Bad: Mixing IO and pure computation
processWith :: String -> IO String
processWith s = do
    putStrLn "Processing..."  -- Unnecessary IO
    return (pureComputation s)
```

1. **Use Resource Management**

```haskell
-- Good: Using bracket
withResource :: IO a
withResource = bracket
    acquire
    release
    use

-- Better than:
manual :: IO a
manual = do
    resource <- acquire
    result <- use resource
    release resource
    return result
```

3. **Handle Errors Appropriately**

```haskell
-- Good: Proper error handling
safeOperation :: IO (Either String Result)
safeOperation = do
    result <- try someOperation
    return $ case result of
        Right x -> Right x
        Left ex -> Left (show ex)
```

## Next Steps

Now that you understand IO and effects, you can:
- Build real-world applications
- Work with files and network resources
- Create interactive programs
- Handle errors and exceptions properly
- Manage program state effectively

Remember: Keep your IO code separate from your pure code, and use Haskell's type system to make effects explicit and manageable! 