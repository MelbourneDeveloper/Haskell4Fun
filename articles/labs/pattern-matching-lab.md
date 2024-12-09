# Lab: Pattern Matching Adventure 🎮

Welcome to an interactive lab where we'll learn pattern matching through a text-based adventure game! You'll write Haskell code to handle different game scenarios using pattern matching.

## Setup

Try out the code samples in our interactive Haskell editor below. Each challenge builds upon the previous one.

### Challenge 1: Basic Item Matching 🎒

Let's start by defining the `Item` type.

```haskell
data Item = Sword Int    -- Int represents damage
          | Shield Int   -- Int represents defense
          | Potion Int   -- Int represents healing amount
          | Key String   -- String represents the door it opens
          deriving Show

-- Fill in the code here

main :: IO ()
main = do
    let inventory = [Sword 50,
                    Shield 30,
                    Potion 100,
                    Key "Dungeon"]
    putStrLn "Inventory contents:"
    mapM_ (putStrLn . describeItem) inventory
```

<div class="challenge-box">
🎯 Your Challenge:
Complete the describeItem function to return descriptive strings for each item:
- Sword should return "A sword that deals X damage"
- Shield should return "A shield with X defense"
- Potion should return "A healing potion that restores X health"
- Key should return "A key that opens the X door"
</div>

<iframe
  frameBorder="0"
  height="450px"
  src="https://onecompiler.com/haskell?hideNewFileOption=true"
  width="100%"
></iframe>

<div class="solution-toggle">
<details>
<summary>👀 See Solution</summary>

```haskell
data Item = Sword Int    -- Int represents damage
          | Shield Int   -- Int represents defense
          | Potion Int   -- Int represents healing amount
          | Key String   -- String represents the door it opens
          deriving Show

describeItem :: Item -> String
describeItem (Sword damage) = "A sword that deals " ++ show damage ++ " damage"
describeItem (Shield defense) = "A shield with " ++ show defense ++ " defense"
describeItem (Potion healing) = "A healing potion that restores " ++ show healing ++ " health"
describeItem (Key doorName) = "A key that opens the " ++ doorName ++ " door"

main :: IO ()
main = do
    let inventory = [Sword 50,
                    Shield 30,
                    Potion 100,
                    Key "Dungeon"]
    putStrLn "Inventory contents:"
    mapM_ (putStrLn . describeItem) inventory
```
</details>
</div>

### Challenge 2: Combat Scenarios ⚔️

Now let's handle combat scenarios using pattern matching!

```haskell
data Attack = Slash | Block | Dodge | UsePotion
    deriving Show
data Enemy = Goblin Int | Dragon Int  -- Int represents health
    deriving Show

-- Fill in the code here

main :: IO ()
main = do
    putStrLn "Combat scenarios:"
    putStrLn $ handleCombat Slash (Goblin 10)
    putStrLn $ handleCombat Block (Dragon 100)
    putStrLn $ handleCombat Dodge (Goblin 5)
```

<div class="challenge-box">
🎯 Your Challenge:
Complete the handleCombat function to return appropriate messages for each combat scenario:
- Slashing should have different effects on Goblins vs Dragons
- Blocking should work against any enemy
- Dodging should avoid all damage
- Using a potion should restore health
</div>

<iframe
  frameBorder="0"
  height="450px"
  src="https://onecompiler.com/haskell?hideNewFileOption=true"
  width="100%"
></iframe>

<div class="solution-toggle">
<details>
<summary>👀 See Solution</summary>

```haskell
data Attack = Slash | Block | Dodge | UsePotion
    deriving Show
data Enemy = Goblin Int | Dragon Int  -- Int represents health
    deriving Show

handleCombat :: Attack -> Enemy -> String
handleCombat Slash (Goblin hp) = "You slash the Goblin for heavy damage!"
handleCombat Slash (Dragon hp) = "The Dragon's scales deflect most of your slash."
handleCombat Block _ = "You raise your shield, reducing incoming damage."
handleCombat Dodge _ = "You nimbly dodge the attack!"
handleCombat UsePotion _ = "You drink the potion, restoring health."

main :: IO ()
main = do
    putStrLn "Combat scenarios:"
    putStrLn $ handleCombat Slash (Goblin 10)
    putStrLn $ handleCombat Block (Dragon 100)
    putStrLn $ handleCombat Dodge (Goblin 5)
```
</details>
</div>

### Challenge 3: Advanced Pattern Guards 🛡️

```haskell
data Item = Sword Int | Shield Int | Potion Int | Key String
    deriving Show

data Enemy = Dragon Int | Goblin Int
    deriving Show

data Attack = Slash | Block | Dodge
    deriving Show

data PlayerState = PlayerState 
    { health :: Int
    , armor :: Int
    , inventory :: [Item]
    } deriving Show

-- Fill in the code here

main :: IO ()
main = do
    let player = PlayerState 100 5 [Sword 10, Shield 5, Potion 20]
    putStrLn "Damage calculations:"
    print $ calculateDamage (Dragon 100) player Block
    print $ calculateDamage (Goblin 20) player Dodge
    print $ calculateDamage (Dragon 50) player Slash
```

<div class="challenge-box">
🎯 Your Challenge:
Complete the calculateDamage function to:
- Use pattern guards to check if the player has a shield
- Calculate different damage amounts based on enemy type and shield status
- Handle dodge attacks (should result in 0 damage)
- Implement a default damage case
</div>

<iframe
  frameBorder="0"
  height="450px"
  src="https://onecompiler.com/haskell?hideNewFileOption=true"
  width="100%"
></iframe>

<div class="solution-toggle">
<details>
<summary>👀 See Solution</summary>

```haskell
data Item = Sword Int | Shield Int | Potion Int | Key String
    deriving Show

data Enemy = Dragon Int | Goblin Int
    deriving Show

data Attack = Slash | Block | Dodge
    deriving Show

data PlayerState = PlayerState 
    { health :: Int
    , armor :: Int
    , inventory :: [Item]
    } deriving Show

calculateDamage :: Enemy -> PlayerState -> Attack -> Int
calculateDamage (Dragon _) player _ | hasShield player = 20
                                   | otherwise = 40
calculateDamage (Goblin _) player Block | hasShield player = 5
                                       | otherwise = 10
calculateDamage _ player Dodge = 0
calculateDamage _ player _ = 15

hasShield :: PlayerState -> Bool
hasShield player = any isShield (inventory player)
    where
        isShield (Shield _) = True
        isShield _ = False

main :: IO ()
main = do
    let player = PlayerState 100 5 [Sword 10, Shield 5, Potion 20]
    putStrLn "Damage calculations:"
    print $ calculateDamage (Dragon 100) player Block
    print $ calculateDamage (Goblin 20) player Dodge
    print $ calculateDamage (Dragon 50) player Slash
```
</details>
</div>

## Testing Your Knowledge 🧪

Try these scenarios in the interactive editor:

1. Create a player with different items
2. Test combat against different enemies
3. Experiment with various attack combinations

```haskell
-- Example test scenario
testPlayer = PlayerState 100 5 [Sword 10, Shield 5, Potion 20]
testCombat = handleCombat Slash (Dragon 200)
```

## Bonus Challenge 🌟

Create a mini text adventure that uses all the patterns we've learned:

```haskell
data Room = Room 
    { description :: String
    , enemies :: [Enemy]
    , items :: [Item]
    }

exploreRoom :: Room -> PlayerState -> String
exploreRoom = -- Your implementation here
```

<div class="challenge-box">
🎯 Your Challenge:
Implement exploreRoom to:
1. Describe the room
2. List available items
3. Warn about enemies
4. Suggest possible actions
</div>

<iframe
  frameBorder="0"
  height="450px"
  src="https://onecompiler.com/haskell?hideNewFileOption=true"
  width="100%"
></iframe>

## Next Steps 🚀

- Try combining multiple pattern matching techniques
- Create more complex game scenarios
- Share your solutions with other learners
- Experiment with different room layouts and enemy combinations

Remember: Pattern matching is about expressing complex logic in a clear, readable way. Keep practicing and experimenting with different patterns!

<style>
.challenge-box {
    background: #f0f7ff;
    border-left: 4px solid #0066cc;
    padding: 1rem;
    margin: 1rem 0;
    border-radius: 4px;
}

.interactive-exercise {
    background: #fff3e0;
    border-left: 4px solid #ff9800;
    padding: 1rem;
    margin: 1rem 0;
    border-radius: 4px;
}

.solution-toggle {
    margin: 1rem 0;
}
</style> 