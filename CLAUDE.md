# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

UIH is a Haskell GUI library with SDL2 backend that provides multiple abstraction layers for building user interfaces. The project follows a layered architecture with different UI approaches:

1. **Hatto** - React-inspired stateful approach with IO monad integration
2. **PicoUI** - Multi-layered architecture with abstract widgets, reactive signals, and raw SDL rendering

## Build Commands

This is a Stack-based Haskell project. Common commands:

- `stack build` - Build the project
- `stack run` - Run the main executable
- `stack run t01` - Run the 01Test example
- `stack run ng` - Run the NewGenTest example  
- `stack test` - Run tests
- `stack ghci` - Start GHCi REPL

## Dependencies

The project requires SDL2 libraries to be installed:

**macOS:**
```bash
brew install sdl2
brew install sdl2_ttf
brew install sdl2_gfx
```

**Linux:**
```bash
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig/
```

## Architecture

### Three-Layer UI Architecture

The project implements a three-layer abstraction:

1. **Declarative Setup** - High-level widget definitions
2. **Internal Representation** - Abstract widgets with calculated dimensions, styles, and state management via reactive variables
3. **Low-level Rendering** - SDL-specific efficient rendering primitives

### Key Components

**UI.Hatto.*** - Simplified React-like approach:
- `App.hs` - Main application wrapper with SDL initialization (`bracketHatto`)
- `Widgets.hs` - Widget definitions and state management
- `Events.hs` - Event handling system
- `SDL/` - SDL-specific rendering and font management

**UI.PicoUI.*** - Advanced multi-layered system:
- `EventLoop.hs` - Main SDL event loop and state management (`runSDLIO`)
- `Middle/AbstractWidgets.hs` - High-level widget definitions independent of rendering
- `Raw/Widgets.hs` - Low-level SDL widgets with caching (`SDLElement` types)
- `Raw/WidgetCompiler.hs` - Compilation from abstract to SDL widgets
- `Reactive/` - FRP-style reactive signals and widget behaviors

### State Management

The system uses two complementary monads:
- **ManagerMonad** - Abstract widget state with reactive variables
- **SDLIO Monad** - Low-level SDL operations with smart caching

### Event System

Events flow from SDL through multiple abstraction layers:
- Raw SDL events → High-level event types → Widget-specific handlers
- Region-based event routing using (x,y,w,h) coordinates
- Reactive signals for FRP-style event handling

## Development Notes

- The project is in active development exploring different UI paradigms
- Current focus is on the reactive widget system in PicoUI
- Font handling includes system font location detection for cross-platform support
- Widgets support focus management, hover states, and text editing capabilities