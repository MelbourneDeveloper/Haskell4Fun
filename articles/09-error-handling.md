# Error Handling

Effective error handling is crucial for building robust applications. It ensures that your program can gracefully handle unexpected situations without crashing.

## Techniques
- **Try-Catch Blocks**: Capture and handle exceptions.
- **Result Objects**: Return success or failure states.
- **Logging**: Keep track of errors for debugging purposes.

## Best Practices
- **Be Specific**: Catch specific exceptions rather than a generic catch-all.
- **Fail Fast**: Detect and handle errors as early as possible.
- **Clean Up**: Ensure resources are released properly in case of errors.

## Example

```haskell
import System.IO
import System.Directory
import Control.Exception
import Data.Either

-- Safe file reading using Either
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left "File does not exist"
        else Right <$> readFile path
```

## Advanced Techniques

### Using `Either` for Composable Error Handling

The `Either` type allows you to chain multiple operations that may fail, propagating errors without deeply nested code.

```haskell
-- Function to parse integer from string
parseInt :: String -> Either String Int
parseInt s =
    case reads s of
        [(n, "")] -> Right n
        _         -> Left "Invalid integer"

-- Function to divide two integers safely
safeDivide :: Int -> Int -> Either String Int
safeDivide _ 0 = Left "Division by zero is not allowed"
safeDivide x y = Right (x `div` y)

-- Chaining operations
processInput :: String -> String -> Either String Int
processInput input1 input2 = do
    num1 <- parseInt input1
    num2 <- parseInt input2
    safeDivide num1 num2
```

### Leveraging the `ExceptT` Monad Transformer

For more complex applications, combining `Either` with monad transformers like `ExceptT` can simplify error handling in IO operations.

```haskell
import Control.Monad.Except

type AppM = ExceptT String IO

-- Safe file reading within AppM
readFileApp :: FilePath -> AppM String
readFileApp path = do
    exists <- liftIO $ doesFileExist path
    if not exists
        then throwError "File does not exist"
        else liftIO $ readFile path

-- Running the AppM
runApp :: AppM a -> IO (Either String a)
runApp app = runExceptT app

-- Example usage
main :: IO ()
main = do
    result <- runApp $ do
        content <- readFileApp "data.txt"
        liftIO $ putStrLn content
        return content
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right _  -> return ()
```

## Logging Errors

Integrating logging with error handling helps in diagnosing issues without interrupting the user experience.

```haskell
import System.IO
import Control.Monad.Except
import Control.Monad.IO.Class

type AppM = ExceptT String IO

-- Function to log errors
logError :: String -> AppM ()
logError err = liftIO $ appendFile "error.log" (err ++ "\n")

-- Safe file reading with logging
readFileLogged :: FilePath -> AppM String
readFileLogged path = do
    exists <- liftIO $ doesFileExist path
    if not exists
        then do
            let err = "File not found: " ++ path
            logError err
            throwError err
        else liftIO $ readFile path

-- Example usage
main :: IO ()
main = do
    result <- runExceptT $ readFileLogged "missing.txt"
    case result of
        Left err -> putStrLn "An error occurred. Check the logs."
        Right content -> putStrLn content
```

### Combining `Either` with `Maybe`

Sometimes, you might want to handle operations that can fail in different ways. Combining `Either` with `Maybe` provides flexibility in error representation.

```haskell
-- Function that might return Nothing or an error
findUser :: Int -> Either String (Maybe User)
findUser userId =
    if userId <= 0
        then Left "Invalid user ID"
        else Right $ lookupUserInDatabase userId

-- Handling the combined result
handleFindUser :: Int -> Either String String
handleFindUser userId = do
    maybeUser <- findUser userId
    case maybeUser of
        Nothing    -> Left "User not found"
        Just user -> Right $ "User found: " ++ userName user
```

### Creating Custom Error Types

Defining custom error types allows for more granular and descriptive error handling.

```haskell
-- Define a custom error type
data AppError
    = FileError FilePath String
    | ParseError String
    | NetworkError String
    deriving (Show, Eq)

-- Using the custom error type with Either
readConfig :: FilePath -> IO (Either AppError Config)
readConfig path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left (FileError path "Config file does not exist")
        else do
            content <- readFile path
            case parseConfig content of
                Nothing -> return $ Left (ParseError "Invalid config format")
                Just config -> return $ Right config
```

### Handling IO Errors with `try` and `catch`

For scenarios where you need to handle exceptions that occur during IO operations, Haskell provides the `try` and `catch` functions.

```haskell
import Control.Exception
import System.IO.Error

-- Function to read a file and handle IO exceptions
readFileWithCatch :: FilePath -> IO (Either String String)
readFileWithCatch path = catch (Right <$> readFile path) handler
  where
    handler :: IOError -> IO (Either String String)
    handler e
        | isDoesNotExistError e = return $ Left "File does not exist"
        | isPermissionError e  = return $ Left "Permission denied"
        | otherwise             = return $ Left "An unknown error occurred"
```

### Using `MonadError` for Enhanced Abstraction

The `MonadError` typeclass provides a way to abstract over error handling, allowing for more flexible and reusable code.

```haskell
import Control.Monad.Except

-- Function using MonadError
readFileMonadError :: (MonadError String m, MonadIO m) => FilePath -> m String
readFileMonadError path = do
    exists <- liftIO $ doesFileExist path
    if not exists
        then throwError "File does not exist"
        else liftIO $ readFile path

-- Running the function within AppM
main :: IO ()
main = do
    result <- runExceptT $ readFileMonadError "data.txt"
    case result of
        Left err    -> putStrLn $ "Error: " ++ err
        Right content -> putStrLn content
```

### Error Aggregation with `Validation`

When performing multiple validations, aggregating errors can provide comprehensive feedback instead of failing fast.

```haskell
import Data.Validation
import Data.Semigroup

-- Define a validation type
type ValidationErrors = [String]

-- Function to validate username
validateUsername :: String -> Validation ValidationErrors String
validateUsername name
    | null name = Failure ["Username cannot be empty"]
    | otherwise = Success name

-- Function to validate password
validatePassword :: String -> Validation ValidationErrors String
validatePassword pass
    | length pass < 8 = Failure ["Password must be at least 8 characters"]
    | otherwise = Success pass

-- Combining validations
validateUser :: String -> String -> Validation ValidationErrors User
validateUser name pass = User <$> validateUsername name <*> validatePassword pass

-- Example usage
example :: Validation ValidationErrors User
example = validateUser "" "short"
-- Results in Failure ["Username cannot be empty", "Password must be at least 8 characters"]
```

## Real-World Applications

### Handling Network Requests

When dealing with network operations, errors can occur due to various reasons such as connectivity issues or invalid responses.

```haskell
import Network.HTTP.Simple
import Control.Monad.Except

type AppM = ExceptT String IO

-- Function to fetch data from an API
fetchData :: String -> AppM String
fetchData url = do
    response <- liftIO $ try (httpLBS (parseRequest_ url)) :: AppM (Either HttpException (Response ByteString))
    case response of
        Left ex -> throwError $ "Network error: " ++ show ex
        Right res ->
            if getResponseStatusCode res /= 200
                then throwError $ "API returned status code " ++ show (getResponseStatusCode res)
                else return $ show (getResponseBody res)
```

### Database Operations

Interacting with databases requires handling errors such as connection failures or query issues.

```haskell
import Database.PostgreSQL.Simple
import Control.Monad.Except

data DBConfig = DBConfig
    { dbHost     :: String
    , dbPort     :: Int
    , dbUser     :: String
    , dbPassword :: String
    , dbName     :: String
    }

type AppM = ExceptT String IO

-- Function to connect to the database
connectDB :: DBConfig -> AppM Connection
connectDB config = do
    let connectInfo = defaultConnectInfo
            { connectHost     = dbHost config
            , connectPort     = fromIntegral (dbPort config)
            , connectUser     = dbUser config
            , connectPassword = dbPassword config
            , connectDatabase = dbName config
            }
    conn <- liftIO $ try (connect connectInfo) :: AppM (Either SqlError Connection)
    case conn of
        Left err -> throwError $ "Database connection error: " ++ show err
        Right connection -> return connection

-- Function to execute a query
getUsers :: Connection -> AppM [User]
getUsers conn = do
    result <- liftIO $ try (query_ conn "SELECT * FROM users") :: AppM (Either SqlError [User])
    case result of
        Left err -> throwError $ "Query error: " ++ show err
        Right users -> return users
```

### Configuration Validation

Validating configuration files ensures that your application has all the necessary settings before running.

```haskell
import Data.Aeson
import Control.Monad.Except

data Config = Config
    { configPort    :: Int
    , configHost    :: String
    , configTimeout :: Int
    } deriving (Show, Generic)

instance FromJSON Config

type AppM = ExceptT String IO

-- Function to validate configuration
validateConfig :: ByteString -> AppM Config
validateConfig content =
    case eitherDecode content of
        Left err    -> throwError $ "Config parsing error: " ++ err
        Right config -> do
            when (configPort config <= 0) $
                throwError "Config validation error: Port must be positive"
            when (configTimeout config < 0) $
                throwError "Config validation error: Timeout cannot be negative"
            return config

-- Example usage
main :: IO ()
main = do
    content <- B.readFile "config.json"
    result <- runExceptT $ validateConfig content
    case result of
        Left err     -> putStrLn err
        Right config -> putStrLn $ "Config loaded: " ++ show config
```

## Conclusion

Effective error handling is vital for creating reliable and maintainable applications. By using techniques like `Either`, `ExceptT`, custom error types, and proper logging, you can manage errors gracefully and ensure that your application remains robust even in the face of unexpected issues.

Implementing these strategies in Haskell not only makes your code safer but also more expressive, clearly delineating the success and failure paths of your functions. Embrace comprehensive error handling to enhance the quality and reliability of your software projects.
