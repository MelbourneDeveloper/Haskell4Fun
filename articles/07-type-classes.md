# Type Classes in Haskell

Type classes provide a principled approach to ad-hoc polymorphism in Haskell. They define interfaces that types can implement, enabling both code reuse and type safety through constrained parametric polymorphism.

## Table of Contents
- [Fundamentals](#fundamentals)
- [Standard Type Classes](#standard-type-classes)
- [Defining Type Classes](#defining-type-classes)
- [Type Class Instances](#type-class-instances)
- [Type Class Constraints](#type-class-constraints)
- [Advanced Type Classes](#advanced-type-classes)
- [Type Class Design](#type-class-design)
- [Common Patterns](#common-patterns)

## Fundamentals

Type classes define a set of operations that types can implement:

```haskell
-- The Eq type class (simplified)
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    
    -- Default implementations using minimal complete definition
    x /= y = not (x == y)
    x == y = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}
```

## Standard Type Classes

### Eq and Ord - Comparisons

```haskell
-- Custom type with derived instances
data Color = Red | Green | Blue
    deriving (Eq, Ord, Show)

-- Manual implementation for understanding
instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

instance Ord Color where
    compare Red   Red   = EQ
    compare Red   _     = LT
    compare Green Blue  = LT
    compare Green Red   = GT
    compare Green Green = EQ
    compare Blue  _     = GT
```

### Functor, Applicative, and Monad

```haskell
-- Understanding the hierarchy
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    (<$) = fmap . const

class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    
    -- Laws:
    -- return a >>= f  ≡  f a
    -- m >>= return    ≡  m
    -- (m >>= f) >>= g ≡  m >>= (\x -> f x >>= g)
```

## Defining Type Classes

### Basic Type Class Definition

```haskell
-- Type class for JSON serialization
class ToJSON a where
    -- | Convert a value to a JSON string
    toJSON :: a -> String
    -- | Convert a value to pretty-printed JSON
    toPrettyJSON :: a -> String
    toPrettyJSON = toJSON  -- Default implementation

-- Instances for basic types
instance ToJSON Int where
    toJSON = show

instance ToJSON Bool where
    toJSON True  = "true"
    toJSON False = "false"

instance ToJSON String where
    toJSON s = "\"" ++ concatMap escape s ++ "\""
      where
        escape '"'  = "\\\""
        escape '\\' = "\\\\"
        escape c    = [c]
```

### Type Classes with Associated Types

```haskell
{-# LANGUAGE TypeFamilies #-}

class Collection c where
    type Element c
    empty   :: c
    insert  :: Element c -> c -> c
    member  :: Element c -> c -> Bool
    toList  :: c -> [Element c]

instance Collection [a] where
    type Element [a] = a
    empty = []
    insert = (:)
    member = elem
    toList = id

instance Ord a => Collection (Set.Set a) where
    type Element (Set.Set a) = a
    empty = Set.empty
    insert = Set.insert
    member = Set.member
    toList = Set.toList
```

## Type Class Instances

### Instance Contexts

```haskell
-- Instance with context
instance Ord a => ToJSON [a] where
    toJSON xs = "[" ++ 
        intercalate "," (map toJSON (sort xs)) ++ "]"

-- Nested instance with multiple contexts
instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Left a)  = "{\"Left\": "  ++ toJSON a ++ "}"
    toJSON (Right b) = "{\"Right\": " ++ toJSON b ++ "}"
```

### Newtype Deriving

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Deriving instances automatically
newtype Age = Age { getAge :: Int }
    deriving (Eq, Ord, Show, Num)

-- Custom newtype with selective derivation
newtype NonEmptyText = NonEmptyText Text
    deriving (Semigroup)  -- But not Monoid!

instance Show NonEmptyText where
    show (NonEmptyText t) = show t
```

## Type Class Constraints

### Multiple Constraints

```haskell
-- Function with multiple constraints
prettyPrint :: (Show a, Ord a, Bounded a) => [a] -> String
prettyPrint xs = unlines
    [ "Min: " ++ show (minimum xs)
    , "Max: " ++ show (maximum xs)
    , "Sorted: " ++ show (sort xs)
    ]

-- Type class with superclass constraints
class (Eq a, Show a) => SafeEnum a where
    safeSucc :: a -> Maybe a
    safePred :: a -> Maybe a
    
    toEnum   :: Int -> Maybe a
    fromEnum :: a -> Int
```

## Advanced Type Classes

### Multi-Parameter Type Classes

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Convertible a b | a -> b where
    convert :: a -> b
    
instance Convertible String Int where
    convert = read
    
instance Convertible Int Double where
    convert = fromIntegral

-- Usage
convertVia :: (Convertible a b, Convertible b c) => a -> c
convertVia = convert . convert
```

### Type Families

```haskell
{-# LANGUAGE TypeFamilies #-}

class Container c where
    type Key c
    type Value c
    
    lookup  :: Key c -> c -> Maybe (Value c)
    insert  :: Key c -> Value c -> c -> c
    delete  :: Key c -> c -> c

instance Ord k => Container (Map.Map k v) where
    type Key (Map.Map k v)   = k
    type Value (Map.Map k v) = v
    
    lookup  = Map.lookup
    insert  = Map.insert
    delete  = Map.delete
```

## Type Class Design

### Laws and Properties

```haskell
class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    
    -- Laws:
    -- mempty `mappend` x = x                          -- Left identity
    -- x `mappend` mempty = x                          -- Right identity
    -- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)  -- Associativity
    
    -- Default implementation
    mconcat = foldr mappend mempty

-- Example: Sum monoid
newtype Sum a = Sum { getSum :: a }

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)
```

## Common Patterns

### Smart Constructors with Type Classes

```haskell
class Validatable a where
    validate :: a -> Either String a

newtype NonEmptyList a = NEL { getNEL :: [a] }

instance Validatable (NonEmptyList a) where
    validate (NEL []) = Left "List cannot be empty"
    validate nel     = Right nel

-- Smart constructor
mkNonEmptyList :: [a] -> Either String (NonEmptyList a)
mkNonEmptyList = validate . NEL

-- Usage with type classes
class Measurable a where
    measure :: a -> Double

instance Measurable (NonEmptyList Double) where
    measure = mean . getNEL
      where
        mean xs = sum xs / fromIntegral (length xs)
```