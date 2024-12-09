# Immutable Records in Haskell

Immutable records are a fundamental concept in functional programming, representing data structures whose values cannot be modified after creation. In Haskell, all data structures are immutable by default, making it an excellent language for exploring and implementing immutable record patterns.

## Table of Contents
- [Basic Concepts](#basic-concepts)
- [Record Syntax](#record-syntax)
- [Working with Records](#working-with-records)
- [Pattern Matching](#pattern-matching)
- [Updating Records](#updating-records)
- [Advanced Patterns](#advanced-patterns)
- [Best Practices](#best-practices)

## Basic Concepts

Immutable records provide several key benefits:
- Thread safety
- Predictable behavior
- Easier debugging
- Simpler testing
- Referential transparency

Here's a simple example of a record in Haskell:

```haskell
data Person = Person
    { name    :: String
    , age     :: Int
    , address :: String
    } deriving (Show, Eq)

-- Creating a person
john :: Person
john = Person
    { name    = "John Doe"
    , age     = 30
    , address = "123 Main St"
    }
```

## Record Syntax

Haskell provides convenient syntax for working with records:

```haskell
-- Record with type annotations
data Employee = Employee
    { employeeId   :: !Int       -- The ! makes the field strict
    , employeeName :: !String
    , department   :: !String
    , salary      :: !Double
    } deriving (Show, Eq)

-- Creating an employee
createEmployee :: Int -> String -> String -> Double -> Employee
createEmployee id name dept sal = Employee
    { employeeId   = id
    , employeeName = name
    , department   = dept
    , salary      = sal
    }
```

## Working with Records

### Accessing Fields

```haskell
-- Using record accessors
getEmployeeName :: Employee -> String
getEmployeeName = employeeName

calculateBonus :: Employee -> Double
calculateBonus emp = salary emp * 0.1

-- Pattern matching with records
showEmployee :: Employee -> String
showEmployee Employee{employeeName = name, department = dept} =
    name ++ " works in " ++ dept
```

### Composing Records

```haskell
data Address = Address
    { street  :: String
    , city    :: String
    , country :: String
    } deriving (Show, Eq)

data Customer = Customer
    { customerId      :: Int
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show, Eq)

-- Creating nested records
newCustomer :: Customer
newCustomer = Customer
    { customerId = 1
    , customerName = "Alice Smith"
    , customerAddress = Address
        { street = "456 Oak Road"
        , city = "Springfield"
        , country = "USA"
        }
    }
```

## Pattern Matching

Pattern matching with records provides a powerful way to destructure data:

```haskell
data Result a = Success a | Error String
    deriving (Show, Eq)

processResult :: Result Employee -> String
processResult (Success emp@Employee{employeeName = name}) =
    "Found employee: " ++ name
processResult (Error msg) =
    "Error: " ++ msg

-- Using guards with records
isHighPaid :: Employee -> Bool
isHighPaid Employee{salary = s}
    | s > 100000 = True
    | otherwise  = False
```

## Updating Records

Since records are immutable, "updating" means creating a new record with modified values:

```haskell
-- Creating a new record with updated fields
giveRaise :: Double -> Employee -> Employee
giveRaise amount emp = emp { salary = salary emp + amount }

-- Updating nested records
updateAddress :: String -> Customer -> Customer
updateAddress newStreet customer = customer
    { customerAddress = (customerAddress customer)
        { street = newStreet }
    }
```

## Advanced Patterns

### Using Lenses

Lenses provide a more convenient way to work with nested records:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Company = Company
    { _companyName    :: String
    , _companyAddress :: Address
    , _employees      :: [Employee]
    } deriving (Show, Eq)

makeLenses ''Company

-- Using lenses to update nested fields
updateCompanyStreet :: String -> Company -> Company
updateCompanyStreet newStreet = over (companyAddress . street) (const newStreet)
```

### Smart Constructors

Smart constructors ensure data validity:

```haskell
data ValidatedEmployee = ValidatedEmployee
    { validatedId   :: Int
    , validatedName :: String
    , validatedAge  :: Int
    } deriving (Show, Eq)

-- Smart constructor with validation
makeEmployee :: Int -> String -> Int -> Either String ValidatedEmployee
makeEmployee id name age
    | id <= 0 = Left "Invalid ID"
    | null name = Left "Name cannot be empty"
    | age < 18 = Left "Employee must be at least 18"
    | otherwise = Right $ ValidatedEmployee id name age
```

## Best Practices

### 1. Use Strict Fields

```haskell
data Configuration = Configuration
    { !configHost :: String  -- Strict field
    , !configPort :: Int     -- Strict field
    } deriving (Show, Eq)
```

### 2. Implement Default Values

```haskell
data Settings = Settings
    { settingsTimeout :: Int
    , settingsRetries :: Int
    } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings
    { settingsTimeout = 30
    , settingsRetries = 3
    }
```

### 3. Type Safety with Phantom Types

```haskell
data Status = Draft | Published
data Article s = Article
    { articleTitle   :: String
    , articleContent :: String
    } deriving (Show, Eq)

-- Type-safe operations
publish :: Article Draft -> Article Published
publish (Article title content) = Article title content
```

### 4. Documentation

```haskell
-- | Represents a user in the system
data User = User
    { -- | Unique identifier for the user
      userId        :: Int
      -- | User's full name
    , userName      :: String
      -- | User's email address
    , userEmail     :: String
      -- | Whether the user's email is verified
    , emailVerified :: Bool
    } deriving (Show, Eq)
```

## Conclusion

Immutable records in Haskell provide a robust foundation for building reliable and maintainable applications. By embracing immutability and leveraging Haskell's powerful type system, you can create safer, more predictable code that's easier to reason about and maintain.

Key takeaways:
- Records are immutable by default in Haskell
- Use record syntax for clear and maintainable code
- Leverage pattern matching for elegant data access
- Consider using lenses for complex nested structures
- Implement smart constructors for data validation
- Use strict fields when appropriate
- Document your record types thoroughly

Remember that immutability is not a limitation but a powerful tool that helps prevent bugs and makes your code more reliable and easier to understand.
