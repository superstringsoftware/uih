# UIH-Elm Examples Documentation

This document provides detailed explanations of all UIH-Elm examples, showing how the Elm architecture works in practice.

## Table of Contents

1. [Counter Example](#counter-example) - Basic state management and user interaction
2. [Future Examples](#future-examples)

---

## Counter Example

**Files:** `Counter.hs`, `TestCounter.hs`  
**Demonstrates:** Basic Elm architecture, state management, type-safe messages, effect system

### Overview

The Counter example is the foundational UIH-Elm application that demonstrates the core Elm architecture pattern. It implements a simple counter with increment, decrement, and reset functionality.

### Architecture Components

#### 1. Model (Application State)
```haskell
data CounterModel = CounterModel { count :: Int } 
  deriving (Show, Eq)
```

**Purpose:** Single source of truth for all application state
- **Immutable:** Never modified directly, only through pure functions
- **Simple:** Contains only the essential data needed
- **Serializable:** Can be easily saved/restored for debugging

#### 2. Messages (User Interactions)
```haskell
data CounterMsg 
  = Increment  -- User clicked "+" button
  | Decrement  -- User clicked "-" button  
  | Reset      -- User clicked "Reset" button
  deriving (Show, Eq)
```

**Purpose:** Represent all possible user interactions as data
- **Exhaustive:** Every possible user action has a corresponding message
- **Type-safe:** Impossible to send wrong message type to wrong component
- **Serializable:** Can be logged, replayed, or sent over network

#### 3. App Definition
```haskell
counterApp :: App CounterModel CounterMsg
counterApp = mkApp
  -- Initial state and startup effects
  (CounterModel 0, [Log "Counter initialized"])
  -- View function: state → UI
  viewCounter
  -- Update function: message → state → (new state, effects)
  updateCounter
```

### The Elm Cycle

#### Step 1: Initialization
```haskell
-- Starting state
init: (CounterModel 0, [Log "Counter initialized"])
```
- Creates initial state with count = 0
- Queues a log effect to run during startup

#### Step 2: View Function (State → UI)
```haskell
viewCounter :: CounterModel -> Widget CounterMsg
viewCounter model = 
  column
    [ text "UIH-Elm Counter" `withStyle` titleStyle
    , spacer 0 20
    , text (pack $ show $ count model) `withStyle` centerStyle  -- ← State displayed here
    , spacer 0 10
    , row
        [ button "-" Decrement     -- ← Clicking generates Decrement message
        , spacer 10 0
        , button "+" Increment     -- ← Clicking generates Increment message
        , spacer 10 0
        , button "Reset" Reset     -- ← Clicking generates Reset message
        ]
    ] `withStyle` centerStyle
```

**Key Points:**
- **Pure function:** Given the same state, always produces the same UI
- **No side effects:** Just builds a widget tree description
- **Message wiring:** Buttons specify which message to send when clicked
- **State integration:** Current count value is displayed in the UI

#### Step 3: Update Function (Message + State → New State + Effects)
```haskell
updateCounter :: CounterMsg → CounterModel → (CounterModel, [Effect CounterMsg])
updateCounter msg model = case msg of
  Increment → 
    let newModel = model { count = count model + 1 }
    in (newModel, [Log $ "Incremented to " <> pack (show $ count newModel)])
    
  Decrement → 
    let newModel = model { count = count model - 1 }  
    in (newModel, [Log $ "Decremented to " <> pack (show $ count newModel)])
    
  Reset →
    let newModel = model { count = 0 }
    in (newModel, [Log "Reset to 0"])
```

**Key Points:**
- **Pure function:** No side effects, just returns new state and effect descriptions
- **Exhaustive:** Every possible message is handled
- **Explicit effects:** Side effects are returned as data, not executed immediately
- **State immutability:** Creates new state rather than modifying existing

#### Step 4: Effect Execution
```haskell
executeEffect :: Effect msg → IO ()
executeEffect effect = case effect of
  None → pure ()
  Log msg → putStrLn $ "LOG: " ++ show msg
  Batch effects → mapM_ executeEffect effects
```

**Key Points:**
- **Isolated side effects:** Only place where IO happens
- **Effect interpretation:** Effects are data that gets interpreted into IO actions
- **Extensible:** Easy to add new effect types (HTTP, file IO, timers, etc.)

### Complete Flow Example

Here's what happens when the user clicks the "+" button:

```
1. User Event: Click on "+" button
   ↓
2. Message Generation: Increment message created
   ↓
3. State Update: updateCounter Increment (CounterModel 5)
   ↓
4. Returns: (CounterModel 6, [Log "Incremented to 6"])
   ↓
5. UI Regeneration: viewCounter (CounterModel 6)
   ↓
6. New Widget Tree: Column[Text("UIH-Elm Counter"), ..., Text("6"), ...]
   ↓
7. Effect Execution: putStrLn "LOG: Incremented to 6"
   ↓
8. UI Render: Display updated interface
```

### Test Output Analysis

When running `TestCounter.hs`, you see:

```
=== Testing UIH-Elm Counter Logic ===
Initial state: CounterModel {count = 0}

Processing: Increment
New state: CounterModel {count = 1}
Effects:
LOG: "Incremented to 1"

Processing: Increment  
New state: CounterModel {count = 2}
Effects:
LOG: "Incremented to 2"

Processing: Decrement
New state: CounterModel {count = 1}  
Effects:
LOG: "Decremented to 1"

Processing: Reset
New state: CounterModel {count = 0}
Effects:
LOG: "Reset to 0"

=== Final Widget Structure ===
Column[Text("UIH-Elm Counter") Spacer(0x20) Text("0") Spacer(0x10) Row[Button("-") Spacer(10x0) Button("+") Spacer(10x0) Button("Reset")]]
```

**What this shows:**
- **Predictable state transitions:** Each message produces expected state change
- **Effect tracking:** All side effects are captured and can be inspected
- **Widget structure:** Final UI structure matches the current state
- **Pure functions:** Everything is deterministic and repeatable

### Widget Tree Structure

The counter produces this widget hierarchy:

```
Column (main container)
├── Text("UIH-Elm Counter")     [title]
├── Spacer(0x20)               [vertical spacing]  
├── Text("0")                  [current count - changes with state]
├── Spacer(0x10)               [vertical spacing]
└── Row (button container)
    ├── Button("-") → Decrement    [sends message when clicked]
    ├── Spacer(10x0)              [horizontal spacing]
    ├── Button("+") → Increment    [sends message when clicked]  
    ├── Spacer(10x0)              [horizontal spacing]
    └── Button("Reset") → Reset    [sends message when clicked]
```

### Key Design Patterns Demonstrated

#### 1. **Single Source of Truth**
- All state lives in `CounterModel`
- UI is derived from state, never contains state
- State can only change through the `update` function

#### 2. **Type-Safe Message Passing**
- Impossible to send wrong message type
- Compiler ensures all messages are handled
- Messages are serializable data, not function calls

#### 3. **Pure Functional Core**
- `view` and `update` functions are pure
- No hidden dependencies or global state
- Easy to test, debug, and reason about

#### 4. **Effect System**
- Side effects are represented as data
- Clear separation between pure logic and IO
- Effects can be inspected, logged, or modified

#### 5. **Immutable State Updates**
- State is never mutated in place
- Each update creates a new state value
- Previous states can be kept for undo/redo

### Benefits of This Architecture

1. **Predictability:** Given the same sequence of messages, always produces the same result
2. **Debuggability:** Can log and replay any sequence of state changes
3. **Testability:** Pure functions are easy to unit test
4. **Time Travel:** Can undo/redo by keeping state history
5. **Hot Reloading:** Can update code while preserving state
6. **Scalability:** Same pattern works for simple and complex applications

### Next Steps

This counter example establishes the foundation. Future examples will build on this pattern to show:
- More complex state structures
- Async operations (HTTP requests)
- Component composition
- Advanced UI patterns
- Real-world applications

---

## Future Examples

*Examples will be documented here as they are created*

### Planned Examples

#### Todo List (`TodoList.hs`) - *Coming in Phase 2*
**Will demonstrate:**
- Complex state management with lists
- HTTP effects for persistence  
- Text input handling
- Conditional rendering
- Local component state

#### Text Editor (`TextEditor.hs`) - *Coming in Phase 4*
**Will demonstrate:**
- Rich text handling
- Cursor management
- Keyboard event handling
- File I/O effects
- Undo/redo with state history

#### Chat Application (`Chat.hs`) - *Coming in Phase 5*
**Will demonstrate:**
- WebSocket effects
- Real-time updates
- Complex UI layouts
- User management
- Message persistence

---

*This document will be updated as new examples are added to the UIH-Elm framework.*