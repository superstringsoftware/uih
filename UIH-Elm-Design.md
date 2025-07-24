# UIH-Elm: A Functional GUI Framework for Haskell

## Executive Summary

UIH-Elm is a proposed redesign of the UIH GUI framework that combines the predictability of Elm architecture with the performance of direct SDL2 access. This document outlines a comprehensive approach to building a production-ready Haskell GUI framework that is both functionally pure and performant.

## Table of Contents

1. [Design Philosophy](#design-philosophy)
2. [Core Architecture](#core-architecture)
3. [Type System](#type-system)
4. [Rendering Pipeline](#rendering-pipeline)
5. [Event System](#event-system)
6. [Layout System](#layout-system)
7. [Effect System](#effect-system)
8. [Performance Strategy](#performance-strategy)
9. [Implementation Plan](#implementation-plan)
10. [Migration Strategy](#migration-strategy)
11. [Examples](#examples)
12. [Comparison with Current Approaches](#comparison-with-current-approaches)

## Design Philosophy

### Core Principles

1. **Functional Purity**: All UI definitions are pure functions that transform state into widget trees
2. **Predictable State Management**: Single source of truth with explicit state transitions
3. **Type Safety**: Leverage Haskell's type system to prevent common GUI programming errors
4. **Performance**: Close-to-metal SDL2 access with intelligent caching and diffing
5. **Composability**: Widgets compose naturally through function composition
6. **Simplicity**: Simple mental model that's easy to understand and debug

### Inspiration Sources

- **Elm Architecture**: Model-View-Update pattern for predictable state management
- **React/Flutter**: Component composition and virtual DOM concepts
- **Brick**: Clean separation between rendering and logic in terminal UIs
- **Immediate Mode GUIs**: Rebuild UI from scratch each frame, but with intelligent caching

## Core Architecture

### The Elm Architecture Pattern

Every UIH-Elm application follows the same pattern:

```
┌─────────────┐    ┌──────────────┐    ┌─────────────┐
│    Model    │───▶│     View     │───▶│   Widget    │
│   (State)   │    │  (Render)    │    │    Tree     │
└─────────────┘    └──────────────┘    └─────────────┘
       ▲                                       │
       │                                       ▼
┌─────────────┐    ┌──────────────┐    ┌─────────────┐
│   Update    │◀───│   Messages   │◀───│   Events    │
│ (State →    │    │              │    │ (Mouse,     │
│  State)     │    │              │    │  Keyboard)  │
└─────────────┘    └──────────────┘    └─────────────┘
```

### Application Definition

```haskell
data App state msg = App
  { appInit   :: (state, [Effect msg])
  , appView   :: state -> Widget msg
  , appUpdate :: msg -> state -> (state, [Effect msg])
  , appStyle  :: StyleSheet
  }
```

## Type System

### Core Widget Type

```haskell
-- Parameterized by message type for type-safe event handling
data Widget msg where
  -- Leaf widgets (primitives)
  Text     :: Text -> Style -> Widget msg
  Button   :: Text -> msg -> Style -> Widget msg
  Input    :: Text -> (Text -> msg) -> Style -> Widget msg
  Image    :: FilePath -> Style -> Widget msg
  Spacer   :: Int -> Int -> Widget msg
  
  -- Container widgets
  Column   :: [Widget msg] -> Style -> Widget msg
  Row      :: [Widget msg] -> Style -> Widget msg
  Stack    :: [Widget msg] -> Style -> Widget msg
  
  -- Interactive wrappers
  Clickable :: Widget msg -> msg -> Widget msg
  Hoverable :: Widget msg -> (Bool -> msg) -> Widget msg
  
  -- Layout widgets
  Flexible :: Int -> Widget msg -> Widget msg
  Positioned :: Position -> Widget msg -> Widget msg
  
  -- Advanced widgets
  Custom   :: (Renderer -> Rect -> IO ()) -> Widget msg

-- Style system
data Style = Style
  { stylePadding       :: Padding
  , styleMargin        :: Margin
  , styleBackground    :: Maybe Background
  , styleForeground    :: Maybe Color
  , styleBorder        :: Maybe Border
  , styleFont          :: Maybe Font
  , styleAlignment     :: Alignment
  , styleFlexGrow      :: Int
  , styleMinSize       :: Maybe (Int, Int)
  , styleMaxSize       :: Maybe (Int, Int)
  }

-- Geometric types
data Rect = Rect !Int !Int !Int !Int  -- x y width height
data Position = Position !Int !Int
data Padding = Padding !Int !Int !Int !Int  -- top right bottom left
```

### Message System

Messages are the only way to communicate user interactions to the application:

```haskell
-- User-defined message types
data TodoMsg 
  = AddTodo Text
  | ToggleTodo Int
  | DeleteTodo Int
  | UpdateInput Text
  
-- System messages (handled by framework)
data SystemMsg
  = WindowResized Int Int
  | WindowClosed
  | FocusChanged (Maybe WidgetId)
```

### Effect System

Side effects are represented as data and executed by the runtime:

```haskell
data Effect msg
  = Timer Duration msg                    -- Set timer
  | HttpRequest Request (Response -> msg) -- HTTP request
  | WriteFile FilePath Text msg           -- File I/O
  | PlaySound FilePath                    -- Audio
  | Log LogLevel Text                     -- Logging
  | Batch [Effect msg]                    -- Combine effects
  | None                                  -- No effect
```

## Rendering Pipeline

### Virtual DOM and Diffing

The rendering pipeline operates in stages:

```
State → Widget Tree → Virtual DOM → Diff → SDL Commands → Screen
```

1. **Build Phase**: `state -> Widget msg` creates a virtual widget tree
2. **Layout Phase**: Calculate positions and sizes for all widgets
3. **Diff Phase**: Compare with previous tree to find minimal changes
4. **Render Phase**: Execute only necessary SDL operations
5. **Cache Phase**: Store textures and computed layouts for reuse

### Layout Engine

Inspired by CSS Flexbox but simplified:

```haskell
data LayoutConstraints = LayoutConstraints
  { minWidth  :: Int
  , maxWidth  :: Int
  , minHeight :: Int
  , maxHeight :: Int
  }

data LayoutResult = LayoutResult
  { layoutRect     :: Rect
  , layoutChildren :: [LayoutResult]
  }

-- Layout algorithm
layout :: Widget msg -> LayoutConstraints -> LayoutResult
```

### Rendering Optimization

- **Texture Caching**: Text and images are rendered to textures once
- **Dirty Rectangles**: Only redraw changed screen regions
- **Batched Operations**: Group similar SDL calls together
- **Culling**: Skip rendering widgets outside viewport
- **Smart Diffing**: Structural sharing to minimize comparisons

## Event System

### Event Flow

```
SDL Events → System Events → Widget Events → Messages → State Updates
```

### Event Handling

Events are processed through a spatial index for efficient hit-testing:

```haskell
-- Spatial indexing for click detection
data EventMap msg = EventMap
  { clickHandlers  :: QuadTree (Rect, msg)
  , hoverHandlers  :: QuadTree (Rect, Bool -> msg)
  , keyHandlers    :: Map KeyCode msg
  , focusOrder     :: [WidgetId]
  }

-- Build event map from widget tree
buildEventMap :: Widget msg -> EventMap msg

-- Process SDL event
handleSDLEvent :: EventMap msg -> SDL.Event -> [msg]
```

### Focus Management

- Tab navigation follows tree order by default
- Explicit focus order can be specified
- Focus styling through CSS-like selectors

## Layout System

### Flexbox-Inspired Layout

```haskell
data FlexDirection = Row | Column
data JustifyContent = FlexStart | FlexEnd | Center | SpaceBetween | SpaceAround
data AlignItems = FlexStart | FlexEnd | Center | Stretch

data FlexStyle = FlexStyle
  { flexDirection  :: FlexDirection
  , justifyContent :: JustifyContent  
  , alignItems     :: AlignItems
  , flexWrap       :: Bool
  }
```

### Layout Examples

```haskell
-- Centered button
centered :: Widget msg -> Widget msg
centered widget = 
  column [widget] (flexStyle { justifyContent = Center, alignItems = Center })

-- Navigation bar
navbar :: [Widget msg] -> Widget msg  
navbar items = 
  row items (flexStyle { justifyContent = SpaceBetween })

-- Responsive grid
grid :: Int -> [Widget msg] -> Widget msg
grid columns items = 
  column (chunksOf columns items |> map (row ?? defaultStyle)) defaultStyle
```

## Effect System

### Effect Execution

Effects are executed by the runtime after state updates:

```haskell
-- Effect interpreter
runEffect :: Effect msg -> IO (Maybe msg)

-- Example: HTTP request
runEffect (HttpRequest req callback) = do
  response <- httpGet req
  pure $ Just (callback response)

-- Example: Timer
runEffect (Timer duration msg) = do
  forkIO $ do
    threadDelay (durationToMicros duration)
    sendMessage msg
  pure Nothing
```

### Common Effect Patterns

```haskell
-- Debounced input
debounce :: Duration -> msg -> Effect msg
debounce delay msg = Timer delay msg

-- HTTP with error handling
httpSafe :: Request -> (Either HttpError Response -> msg) -> Effect msg
httpSafe req callback = HttpRequest req (Right >>> callback) 
                       `catchHttp` (Left >>> callback)

-- Batch multiple effects
batch :: [Effect msg] -> Effect msg
batch = Batch
```

## Performance Strategy

### Benchmarking Targets

- **Startup Time**: < 100ms for simple apps
- **Frame Rate**: 60 FPS with 1000+ widgets
- **Memory Usage**: < 50MB for typical applications
- **Responsiveness**: < 16ms event-to-render latency

### Optimization Techniques

1. **Structural Sharing**: Reuse unchanged widget subtrees
2. **Incremental Layout**: Only recalculate affected regions
3. **Texture Atlases**: Pack small textures efficiently
4. **Worker Threads**: Async texture loading and HTTP
5. **Memory Pools**: Reduce GC pressure
6. **SIMD**: Vectorized color blending operations

### Performance Monitoring

```haskell
data PerformanceStats = PerformanceStats
  { frameTime     :: Double
  , layoutTime    :: Double  
  , renderTime    :: Double
  , widgetCount   :: Int
  , textureMemory :: Int
  }

-- Built-in performance overlay
performanceOverlay :: PerformanceStats -> Widget msg
```

## Implementation Plan

### Phase 1: Foundation (2 weeks)

**Goal**: Basic working widgets with SDL rendering

**Deliverables**:
- Core `Widget` type with Text, Button, Column
- Direct SDL rendering (no optimization)
- Basic mouse click handling
- Simple counter example

**Success Criteria**:
- Counter app runs at 60 FPS
- Click events work correctly
- Clean, readable code

### Phase 2: Elm Architecture (2 weeks)

**Goal**: Full Elm-style state management

**Deliverables**:
- `App` type with init/view/update
- Message system with type safety
- Main application loop
- Todo list example

**Success Criteria**:
- Complex state transitions work correctly
- No runtime errors from type mismatches
- Todo app demonstrates full cycle

### Phase 3: Performance (2 weeks)

**Goal**: Production-ready performance

**Deliverables**:
- Widget tree diffing algorithm
- Texture caching system
- Batched SDL operations
- Performance benchmarks

**Success Criteria**:
- 60 FPS with 1000+ widgets
- < 16ms event latency
- Memory usage stays constant

### Phase 4: Polish (2 weeks)

**Goal**: Full-featured framework

**Deliverables**:
- Advanced layouts (flexbox)
- Effect system implementation
- Text input and focus management
- Comprehensive documentation

**Success Criteria**:
- Feature parity with existing frameworks
- Clean, documented API
- Production-ready examples

## Migration Strategy

### From Current UIH Code

1. **Preserve SDL Infrastructure**: Reuse font loading, renderer setup
2. **Convert Widgets Incrementally**: Transform existing widgets one by one
3. **Bridge Pattern**: Allow old and new widgets to coexist temporarily
4. **Performance Comparison**: Benchmark against current implementation

### Migration Steps

```haskell
-- Step 1: Create compatibility layer
newWidget :: OldWidget -> Widget msg

-- Step 2: Convert applications piece by piece  
migrateApp :: OldApp -> App State Msg

-- Step 3: Remove old code when conversion complete
```

## Examples

### Simple Counter

```haskell
data CounterModel = CounterModel { count :: Int }
data CounterMsg = Increment | Decrement

counterApp :: App CounterModel CounterMsg
counterApp = App
  { appInit = (CounterModel 0, [])
  , appView = viewCounter
  , appUpdate = updateCounter
  , appStyle = defaultStyle
  }

viewCounter :: CounterModel -> Widget CounterMsg
viewCounter model = 
  column
    [ text (show $ count model) titleStyle
    , row 
        [ button "+" Increment buttonStyle
        , button "-" Decrement buttonStyle
        ] defaultStyle
    ] containerStyle

updateCounter :: CounterMsg -> CounterModel -> (CounterModel, [Effect CounterMsg])
updateCounter msg model = case msg of
  Increment -> (model { count = count model + 1 }, [])
  Decrement -> (model { count = count model - 1 }, [])
```

### HTTP Todo List

```haskell
data TodoModel = TodoModel 
  { todos :: [Todo]
  , newTodo :: Text
  , loading :: Bool
  }

data TodoMsg 
  = UpdateInput Text
  | AddTodo
  | TodoAdded (Either HttpError Todo)
  | LoadTodos
  | TodosLoaded (Either HttpError [Todo])

viewTodos :: TodoModel -> Widget TodoMsg
viewTodos model = 
  column
    [ row 
        [ input (newTodo model) UpdateInput inputStyle
        , button "Add" AddTodo buttonStyle
        ] defaultStyle
    , if loading model 
        then text "Loading..." defaultStyle
        else column (map viewTodo $ todos model) defaultStyle
    ] containerStyle

updateTodos :: TodoMsg -> TodoModel -> (TodoModel, [Effect TodoMsg])
updateTodos msg model = case msg of
  UpdateInput text -> 
    (model { newTodo = text }, [])
  AddTodo -> 
    (model { loading = True }, [httpPost "/todos" (newTodo model) TodoAdded])
  TodoAdded (Right todo) -> 
    (model { todos = todo : todos model, loading = False, newTodo = "" }, [])
  -- ... other cases
```

### Custom Widget

```haskell
-- Custom circular progress widget
progressCircle :: Float -> Color -> Widget msg
progressCircle progress color = 
  Custom $ \renderer rect -> do
    let center = rectCenter rect
        radius = min (rectWidth rect) (rectHeight rect) `div` 2
        angle = progress * 2 * pi
    drawCircle renderer center radius color
    drawArc renderer center radius 0 angle (lighten color)
```

## Comparison with Current Approaches

### vs. Hatto (React-inspired)

**Advantages**:
- ✅ Pure functions instead of mutable `IORef`s
- ✅ Type-safe state management
- ✅ Predictable, testable updates
- ✅ Better composition through functional approach

**Trade-offs**:
- ❓ Less direct SDL access (but still available via Custom widget)
- ❓ More structured (less "freedom" to do arbitrary IO)

### vs. PicoUI (Multi-layered FRP)

**Advantages**:
- ✅ Much simpler (1 abstraction layer vs 3)
- ✅ Easier to understand and debug  
- ✅ Better performance (fewer transformations)
- ✅ More predictable (no complex reactive graph)

**Trade-offs**:
- ❓ Less sophisticated reactive features
- ❓ No automatic signal propagation

### vs. Other Haskell GUI Frameworks

**vs. GTK/Qt bindings**:
- ✅ Pure functional approach
- ✅ No foreign marshalling overhead
- ✅ Type-safe event handling
- ❌ Smaller widget library

**vs. Brick (terminal)**:
- ✅ Similar clean architecture
- ✅ Better performance (hardware acceleration)
- ✅ More widget types (images, etc.)
- ❌ More complex (graphics vs. text)

## Conclusion

UIH-Elm represents a significant evolution in Haskell GUI programming. By combining proven functional patterns with modern performance techniques, we can create a framework that is both elegant and practical.

The incremental implementation plan ensures we can validate each design decision with working code, while the migration strategy provides a path forward from the existing UIH codebase.

This approach positions UIH as a competitive alternative to mainstream GUI frameworks while leveraging Haskell's unique strengths in type safety and functional composition.