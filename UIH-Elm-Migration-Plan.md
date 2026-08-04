# UIH to UIH-Elm Migration Implementation Plan

**Status:** Planning Phase  
**Last Updated:** 2025-07-24  
**Current Phase:** Foundation Planning

## Overview

This document tracks the migration from the current multi-layered UIH architecture (Hatto + PicoUI) to the clean, functional UIH-Elm approach based on the Elm architecture pattern.

## Current Codebase Analysis

### Strengths to Preserve ✅
- **SDL2 Infrastructure:** UI.Hatto.SDL.* modules provide solid foundation
- **Font System:** DPI-aware font loading and caching in UI.Hatto.SDL.Fonts
- **Event Foundation:** Basic event handling in UI.PicoUI.Raw.Events
- **Text Input:** Cursor management and basic text editing
- **Rendering Pipeline:** Efficient SDL rendering in UI.PicoUI.Raw.Rendering

### Components to Replace ❌
- **Complex Architecture:** Multi-layer PicoUI system (3 abstraction layers)
- **State Management:** IORef-based mutable state in Hatto widgets
- **Reactive System:** Complex FRP signals in UI.PicoUI.Reactive.*
- **Widget System:** Current widget definitions and compilation

## Implementation Phases

### Phase 1: Foundation (Week 1) ✅
**Goal:** Establish core UIH-Elm types and basic rendering

**Status:** COMPLETED  
**Completion Date:** 2025-07-24

#### New Module Structure
```
src/UI/UIHElm/
├── Core/
│   ├── Widget.hs        -- Core Widget type ✅
│   ├── App.hs           -- App type and main loop ✅
│   ├── Types.hs         -- Basic types (Rect, Position, etc.) ✅
└── Examples/
    └── Counter.hs       -- Simple counter example ✅
    └── TestCounter.hs   -- Test runner ✅
```

#### Week 1 Tasks
- [x] Create `UI.UIHElm.Core.Types` with basic geometric and style types
- [x] Create `UI.UIHElm.Core.Widget` with GADT widget system
- [x] Create `UI.UIHElm.Core.App` with Elm architecture 
- [x] Create working counter example with logic validation
- [x] Ensure `stack build` works correctly

#### Success Criteria ✅
- ✅ Pure functional state management working
- ✅ Type-safe message system operational  
- ✅ Widget tree composition functional
- ✅ Clean, readable code structure
- ✅ Effect system with basic logging

#### Test Results
```
=== Testing UIH-Elm Counter Logic ===
Initial state: CounterModel {count = 0}
Processing: Increment → CounterModel {count = 1} ✅
Processing: Increment → CounterModel {count = 2} ✅  
Processing: Decrement → CounterModel {count = 1} ✅
Processing: Reset → CounterModel {count = 0} ✅

Widget Structure: Column[Text("UIH-Elm Counter") Spacer(0x20) Text("0") ...]
```

### Phase 2: Elm Architecture (Week 2) ⭕
**Goal:** Complete message system and state management

**Status:** Not Started  
**Target Completion:** TBD

#### Week 2 Tasks
- [ ] Effect system implementation
- [ ] Event handling with spatial indexing
- [ ] Main application loop with event processing
- [ ] Todo list example demonstrating full cycle
- [ ] Basic focus management

#### Success Criteria
- Complex state transitions work correctly
- No runtime errors from type mismatches
- Todo app demonstrates full Elm cycle

### Phase 3: Performance (Week 3) ⭕
**Goal:** Widget diffing and rendering optimizations

**Status:** Not Started  
**Target Completion:** TBD

#### Week 3 Tasks
- [ ] Virtual DOM with widget tree diffing
- [ ] Texture caching system
- [ ] Dirty rectangle tracking
- [ ] Batched SDL operations
- [ ] Performance benchmarking

#### Success Criteria
- 60 FPS with 1000+ widgets
- < 16ms event latency
- Memory usage stays constant

### Phase 4: Text Integration (Week 4) ⭕
**Goal:** Integrate comprehensive text support

**Status:** Not Started  
**Target Completion:** TBD

#### Week 4 Tasks
- [ ] Enhanced text widgets with rich formatting
- [ ] Text input state management with selection
- [ ] Font system integration (reuse existing)
- [ ] Text layout engine with wrapping
- [ ] Clipboard integration

### Phase 5: Advanced Features (Week 5) ⭕
**Goal:** Complete feature set and polish

**Status:** Not Started  
**Target Completion:** TBD

#### Week 5 Tasks
- [ ] Advanced layouts (flexbox-inspired)
- [ ] Custom widget support
- [ ] Focus management and keyboard navigation
- [ ] Comprehensive examples
- [ ] Documentation and migration guide

#### Success Criteria
- Feature parity with existing frameworks
- Clean, documented API
- Production-ready examples

## Migration Strategy

### SDL Infrastructure Reuse
**Status:** Identified ✅

```haskell
-- Bridge module to migrate existing SDL code
-- UI.UIHElm.SDL.Bridge
migrateSDLState :: UI.Hatto.Widgets.SDLState -> UI.UIHElm.SDL.State
migrateFont :: (Text, Int) -> FontSpec
```

**Modules to preserve and adapt:**
- `UI.Hatto.SDL.Fonts` → Font loading with DPI scaling
- `UI.Hatto.SDL.Rendering` → Basic rendering primitives  
- `UI.PicoUI.Raw.Rendering` → Advanced rendering operations

### Event System Migration
**Status:** Planned ✅

**Current → New mapping:**
```haskell
-- Current: UI.PicoUI.Raw.Events
data Event = ELeftClick EventSource Int | ...

-- New: UI.UIHElm.Events
data SystemEvent = MouseClick Position | KeyPress KeyCode | ...
buildEventMap :: Widget msg -> EventMap msg
```

### Widget Migration Path
**Status:** Planned ✅

1. **Text widgets:** `WETextLabel` → `Text Text Style`
2. **Input widgets:** StatefulWidget → `TextInput Text (Text -> msg)`
3. **Layout widgets:** Manual positioning → Flexbox-inspired layout

### State Management Migration
**Status:** Planned ✅

**From IORef-based to pure functional:**
```haskell
-- Current: Mutable state with IORef
newStatefulWidget :: s -> (s -> Element) -> (Event -> s -> s) -> m (StatefulWidget m)

-- New: Pure functional with messages
data AppModel = AppModel { ... }
data AppMsg = ...
appUpdate :: AppMsg -> AppModel -> (AppModel, [Effect AppMsg])
```

## Final File Structure

```
src/UI/UIHElm/
├── Core/
│   ├── Widget.hs           -- Core widget types
│   ├── App.hs              -- App type and runtime
│   ├── Style.hs            -- Style system
│   ├── Types.hs            -- Basic types
│   └── Effect.hs           -- Effect system
├── Rendering/
│   ├── SDL.hs              -- SDL backend (migrated)
│   ├── Layout.hs           -- Layout engine
│   ├── Cache.hs            -- Texture caching
│   └── Diff.hs             -- Widget diffing
├── Text/
│   ├── Input.hs            -- Text input widgets
│   ├── Layout.hs           -- Text layout engine
│   ├── Font.hs             -- Font system (migrated)
│   └── Rich.hs             -- Rich text support
├── Widgets/
│   ├── Basic.hs            -- Text, Button, etc.
│   ├── Layout.hs           -- Column, Row, Stack
│   ├── Input.hs            -- TextInput, TextArea
│   └── Custom.hs           -- Custom widget support
└── Examples/
    ├── Counter.hs          -- Simple counter
    ├── TodoList.hs         -- Todo with HTTP
    └── TextEditor.hs       -- Rich text editor
```

## Progress Tracking

### Legend
- ⭕ Not Started
- 🔄 In Progress  
- ✅ Completed
- ❌ Blocked/Issues

### Overall Progress
- **Phase 1:** ✅ 100% (5/5 tasks) - COMPLETED 2025-07-24
- **Phase 2:** ⭕ 0% (0/5 tasks)
- **Phase 3:** ⭕ 0% (0/5 tasks)
- **Phase 4:** ⭕ 0% (0/5 tasks)
- **Phase 5:** ⭕ 0% (0/5 tasks)

**Total Progress:** 20% (5/25 major tasks)

## Key Design Decisions

### Architecture Choices ✅
- **Single abstraction layer** instead of current 3-layer system
- **Pure functional state management** replacing IORef approach
- **Type-safe message system** for all interactions
- **Virtual DOM with diffing** for performance

### Text System Integration ✅
- Reuse existing font loading and DPI scaling
- Add rich text formatting capabilities
- Implement comprehensive text input with selection
- Support clipboard operations and keyboard shortcuts

### Performance Strategy ✅
- Texture caching for text and images
- Widget tree diffing to minimize redraws
- Spatial indexing for efficient event handling
- Batched SDL operations

## Migration Commands

1. **Preserve existing functionality:** Keep `examples/01Test.hs` working
2. **Gradual transition:** New examples use UIH-Elm, old examples remain functional
3. **Build system:** `stack build` should work throughout migration
4. **Performance validation:** Benchmark new vs. current implementation

## Notes and Issues

### Current Issues
- None identified yet

### Design Questions
- None pending

### Performance Benchmarks
- TBD: Baseline measurements needed

---

**Next Steps:** Begin Phase 1 implementation with core Widget types and basic App structure.