# Custom Types and Data Modeling in Haskell

One of Haskell's greatest strengths is its powerful type system. Let's learn how to create our own types to model data effectively!

## Basic Data Types

### Simple Type Aliases

Type aliases make code more readable:

```haskell
type Name = String
type Age = Int
type Email = String

-- Using the aliases
createUser :: Name -> Age -> Email -> User
createUser name age email = User name age email
```

### Newtype Wrappers

`newtype` creates a new type with exactly one constructor and one field:

```haskell
newtype EmailAddress = EmailAddress String
newtype UserId = UserId Integer

-- This prevents mixing up different string-based types
validateEmail :: EmailAddress -> Bool
validateEmail (EmailAddress email) = '@' `elem` email
```

## Data Types with Multiple Constructors

### Simple Enumerations

```haskell
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

data TrafficLight = Red | Yellow | Green
  deriving (Show, Eq)

-- Using enums
isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False
```

### Product Types (Records)

```haskell
data Person = Person
    { personName :: String
    , personAge :: Int
    , personEmail :: String
    } deriving (Show, Eq)

-- Creating and using records
john :: Person
john = Person
    { personName = "John Doe"
    , personAge = 30
    , personEmail = "john@example.com"
    }

-- Accessing fields
getName :: Person -> String
getName = personName

-- Updating records
birthday :: Person -> Person
birthday person = person { personAge = personAge person + 1 }
```

### Sum Types

```haskell
data Shape
    = Circle Double           -- radius
    | Rectangle Double Double -- width and height
    | Triangle Double Double  -- base and height
    deriving (Show, Eq)

-- Working with sum types
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle b h) = (b * h) / 2
```

## Generic Types

### Parameterized Types

```haskell
-- A box that can hold any type
data Box a = Box a deriving (Show)

-- A pair of the same type
data Pair a = Pair a a deriving (Show)

-- Examples
intBox :: Box Int
intBox = Box 42

stringPair :: Pair String
stringPair = Pair "hello" "world"
```

### Maybe and Either

```haskell
-- Maybe for optional values
data Maybe a = Nothing | Just a

-- Either for values of two possible types
data Either a b = Left a | Right b

-- Example usage
findUser :: UserId -> Maybe User
findUser id = 
    if userExists id
        then Just (getUser id)
        else Nothing

validateAge :: Int -> Either String Int
validateAge age
    | age < 0 = Left "Age cannot be negative"
    | age > 150 = Left "Age seems unrealistic"
    | otherwise = Right age
```

## Advanced Type Features

### Recursive Types

```haskell
data List a = Empty | Cons a (List a)
    deriving (Show)

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show)

-- Example: Binary search tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)
```

### Type Constraints

```haskell
-- Type must be comparable
data Ordered a = Ordered a
    deriving (Show)
    
makeOrdered :: Ord a => a -> Ordered a
makeOrdered = Ordered

-- Type must be showable and comparable
data Tagged a = Tagged
    { taggedValue :: a
    , taggedLabel :: String
    } deriving (Show)
```

## Best Practices

### Making Invalid States Unrepresentable

Instead of:
```haskell
data User = User
    { userName :: String
    , userAge :: Int      -- Could be negative!
    , userEmail :: String -- Could be invalid!
    }
```

Better:
```haskell
newtype NonEmptyString = NonEmptyString String
newtype Age = Age Int
newtype EmailAddress = EmailAddress String

data User = User
    { userName :: NonEmptyString
    , userAge :: Age
    , userEmail :: EmailAddress
    }

makeAge :: Int -> Maybe Age
makeAge n
    | n >= 0 && n <= 150 = Just (Age n)
    | otherwise = Nothing

makeEmail :: String -> Maybe EmailAddress
makeEmail s
    | isValidEmail s = Just (EmailAddress s)
    | otherwise = Nothing
```

### Smart Constructors

```haskell
module SafeTypes 
    ( PositiveNumber
    , makePositive
    , getValue
    ) where

newtype PositiveNumber = PositiveNumber Double

-- Only expose the smart constructor, not the data constructor
makePositive :: Double -> Maybe PositiveNumber
makePositive x
    | x > 0 = Just (PositiveNumber x)
    | otherwise = Nothing

getValue :: PositiveNumber -> Double
getValue (PositiveNumber x) = x
```

## Pattern Matching with Custom Types

```haskell
data Result a = Success a | Error String

-- Comprehensive pattern matching
handleResult :: Result a -> String
handleResult (Success x) = "Success: " ++ show x
handleResult (Error msg) = "Error: " ++ msg

-- Guards with pattern matching
describeShape :: Shape -> String
describeShape shape = case shape of
    Circle r | r <= 0 -> "Invalid circle"
            | r < 1 -> "Small circle"
            | r < 5 -> "Medium circle"
            | otherwise -> "Large circle"
    Rectangle w h | w == h -> "Square"
                 | otherwise -> "Rectangle"
    Triangle b h -> "Triangle"
```

## Practical Examples

### Building a Library System

```haskell
data BookStatus = Available | Borrowed | Lost
    deriving (Show, Eq)

data Book = Book
    { bookTitle :: String
    , bookAuthor :: String
    , bookISBN :: String
    , bookStatus :: BookStatus
    } deriving (Show)

data Member = Member
    { memberName :: String
    , memberID :: String
    , borrowedBooks :: [Book]
    } deriving (Show)

-- Library operations
borrowBook :: Book -> Member -> Either String (Book, Member)
borrowBook book member
    | bookStatus book /= Available = 
        Left "Book is not available"
    | length (borrowedBooks member) >= 3 = 
        Left "Member has too many books"
    | otherwise = Right
        ( book { bookStatus = Borrowed }
        , member { borrowedBooks = book : borrowedBooks member }
        )
```

## Next Steps

Now that you understand custom types, you can:
- Design more type-safe APIs
- Create domain-specific type systems
- Use advanced type features like GADTs and type families
- Model complex business logic with the type system

Remember: Haskell's type system is your friend - use it to prevent bugs before they happen! 