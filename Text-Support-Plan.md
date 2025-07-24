# Complete Text Support in UIH-Elm

## Overview

Text support in UIH-Elm will be comprehensive, covering font loading, text rendering, text input, and advanced typography. Building on the existing SDL2-TTF foundation, we'll provide a clean, type-safe API that handles all text-related functionality efficiently.

## Current Foundation Analysis

Your existing code already provides excellent foundations:

**Strengths:**
- ✅ DPI-aware font scaling in `UI.Hatto.SDL.Fonts`
- ✅ Font caching system with Map-based storage
- ✅ Text input event handling in PicoUI
- ✅ Cursor rendering with blink animation
- ✅ Multi-line text support

**Areas for Enhancement:**
- Better font fallback system
- Rich text formatting (bold, italic, colors within text)
- Text selection and clipboard integration
- Internationalization support
- Performance optimization for large text blocks

## Font Management System

### Font Loading and Caching

```haskell
-- Enhanced font system
data FontFamily = FontFamily
  { familyName    :: Text
  , regularPath   :: FilePath
  , boldPath      :: Maybe FilePath
  , italicPath    :: Maybe FilePath
  , boldItalicPath :: Maybe FilePath
  }

data FontStyle = Regular | Bold | Italic | BoldItalic
  deriving (Show, Eq, Ord)

data FontSpec = FontSpec
  { fontFamily :: Text
  , fontSize   :: Int
  , fontStyle  :: FontStyle
  } deriving (Show, Eq, Ord)

-- Global font registry
data FontRegistry = FontRegistry
  { registeredFamilies :: Map Text FontFamily
  , loadedFonts       :: Map FontSpec Font
  , defaultFont       :: FontSpec
  , fallbackFonts     :: [Text]  -- Fallback chain
  }

-- Font loading with fallbacks
loadFont :: FontSpec -> FontRegistry -> IO (Either FontError Font)
loadFontWithFallback :: FontSpec -> FontRegistry -> IO Font

-- System font discovery
discoverSystemFonts :: IO [FontFamily]
registerSystemFonts :: FontRegistry -> IO FontRegistry
```

### Font Configuration

```haskell
-- Configuration-based font setup
data FontConfig = FontConfig
  { configFamilies    :: [FontFamily]
  , configDefault     :: Text
  , configFallbacks   :: [Text]
  , configDPIScaling  :: Bool
  , configSubpixel    :: Bool
  }

-- Load from config file
loadFontConfig :: FilePath -> IO FontConfig
defaultFontConfig :: FontConfig

-- Example config:
defaultFontConfig = FontConfig
  { configFamilies = 
      [ FontFamily "Roboto" "./fonts/Roboto/Roboto-Regular.ttf" 
          (Just "./fonts/Roboto/Roboto-Bold.ttf")
          (Just "./fonts/Roboto/Roboto-Italic.ttf") 
          (Just "./fonts/Roboto/Roboto-BoldItalic.ttf")
      , FontFamily "DejaVu Sans" "/usr/share/fonts/DejaVu/DejaVuSans.ttf" Nothing Nothing Nothing
      ]
  , configDefault = "Roboto"
  , configFallbacks = ["DejaVu Sans", "Arial", "Helvetica"]
  , configDPIScaling = True  
  , configSubpixel = True
  }
```

## Text Widgets

### Basic Text Display

```haskell
-- Enhanced text widget with rich formatting
data TextWidget msg = TextWidget
  { textContent :: RichText
  , textFont    :: FontSpec
  , textAlign   :: TextAlignment
  , textWrap    :: TextWrap
  , textColor   :: Color
  , textMaxWidth :: Maybe Int
  , textMaxLines :: Maybe Int
  }

-- Rich text support
data RichText 
  = PlainText Text
  | StyledText [TextSpan]
  
data TextSpan = TextSpan
  { spanText   :: Text
  , spanFont   :: Maybe FontSpec  -- Override font
  , spanColor  :: Maybe Color     -- Override color
  , spanBG     :: Maybe Color     -- Background highlight
  , spanLink   :: Maybe msg       -- Clickable link
  }

-- Text alignment and wrapping
data TextAlignment = LeftAlign | CenterAlign | RightAlign | JustifyAlign
data TextWrap = NoWrap | WordWrap | CharWrap

-- Widget constructors
text :: Text -> Widget msg
text content = TextWidget (PlainText content) defaultFont LeftAlign WordWrap defaultColor Nothing Nothing

richText :: [TextSpan] -> Widget msg
styledText :: Text -> FontSpec -> Color -> Widget msg
linkText :: Text -> msg -> Widget msg
```

### Text Input Widgets

```haskell
-- Text input with advanced features
data TextInput msg = TextInput
  { inputValue      :: Text
  , inputPlaceholder :: Text
  , inputOnChange   :: Text -> msg
  , inputValidator  :: Text -> Either Text Text  -- Validation
  , inputFilter     :: Char -> Bool              -- Character filter
  , inputMaxLength  :: Maybe Int
  , inputMask       :: Maybe Text                -- Input mask (e.g., "###-##-####")
  , inputStyle      :: InputStyle
  , inputReadOnly   :: Bool
  , inputPassword   :: Bool
  }

data InputStyle = InputStyle
  { inputFont         :: FontSpec
  , inputTextColor    :: Color
  , inputBGColor      :: Color
  , inputBorderColor  :: Color
  , inputFocusColor   :: Color
  , inputPadding      :: Padding
  , inputCursorColor  :: Color
  , inputSelectionBG  :: Color
  }

-- Text input constructors
textInput :: Text -> (Text -> msg) -> Widget msg
passwordInput :: Text -> (Text -> msg) -> Widget msg  
numberInput :: Int -> (Int -> msg) -> Widget msg
emailInput :: Text -> (Text -> msg) -> Widget msg

-- Multi-line text area
textArea :: Text -> (Text -> msg) -> Widget msg
textArea value onChange = TextArea
  { areaValue = value
  , areaOnChange = onChange
  , areaMinRows = 3
  , areaMaxRows = Nothing
  , areaWrap = WordWrap
  , areaStyle = defaultInputStyle
  }
```

### Advanced Text Widgets

```haskell
-- Text editor with syntax highlighting
data TextEditor msg = TextEditor
  { editorContent   :: Text
  , editorLanguage  :: Maybe Language
  , editorTheme     :: SyntaxTheme
  , editorOnChange  :: Text -> msg
  , editorOnCursor  :: (Int, Int) -> msg  -- Line, column
  , editorReadOnly  :: Bool
  , editorShowLines :: Bool
  , editorTabSize   :: Int
  }

-- Syntax highlighting
data Language = Haskell | JavaScript | Python | Markdown
data SyntaxTheme = LightTheme | DarkTheme | CustomTheme ThemeColors

-- Code editor
codeEditor :: Language -> Text -> (Text -> msg) -> Widget msg

-- Markdown viewer
markdownViewer :: Text -> Widget msg
```

## Text Input State Management

### Input State

```haskell
-- Text input state with cursor and selection
data TextInputState = TextInputState
  { inputText      :: Text
  , cursorPos      :: Int           -- Character position
  , selectionStart :: Maybe Int     -- Selection start (if any)
  , selectionEnd   :: Maybe Int     -- Selection end
  , scrollOffset   :: Int           -- Horizontal scroll
  , focused        :: Bool
  , blinkState     :: Bool
  , lastBlink      :: Word32        -- Last blink time
  }

-- Text editing operations
data TextEdit
  = InsertChar Char
  | InsertText Text
  | DeleteChar
  | DeleteSelection
  | MoveCursor CursorMove
  | Select SelectionMove
  | Copy
  | Cut
  | Paste Text

data CursorMove 
  = CharLeft | CharRight 
  | WordLeft | WordRight
  | LineStart | LineEnd
  | LineUp | LineDown

-- Text input update function
updateTextInput :: TextEdit -> TextInputState -> TextInputState
```

### Input Events

```haskell
-- Enhanced event handling for text input
data TextInputEvent
  = TextEntered Text              -- Text input from IME/keyboard
  | KeyPressed KeyCode Modifiers  -- Key press with modifiers
  | KeyReleased KeyCode
  | MouseClick Position
  | MouseDrag Position Position   -- For text selection
  | Focus
  | Blur

-- Key handling
data KeyCode = KeyBackspace | KeyDelete | KeyEnter | KeyTab | KeyEscape 
             | KeyLeft | KeyRight | KeyUp | KeyDown | KeyHome | KeyEnd
             | KeyA | KeyC | KeyV | KeyX | KeyZ | KeyY -- Common shortcuts
             | OtherKey Int

data Modifiers = Modifiers
  { modShift :: Bool
  , modCtrl  :: Bool  
  , modAlt   :: Bool
  , modMeta  :: Bool  -- Cmd on Mac
  }

-- Event processing
processTextInputEvent :: TextInputEvent -> TextInputState -> (TextInputState, [Effect msg])
```

## Text Rendering Pipeline

### Text Layout Engine

```haskell
-- Text layout with line breaking and wrapping
data TextLayout = TextLayout
  { layoutLines    :: [TextLine]
  , layoutBounds   :: Rect
  , layoutBaseline :: Int
  }

data TextLine = TextLine
  { lineText     :: Text
  , lineWidth    :: Int
  , lineHeight   :: Int
  , lineAscent   :: Int
  , lineDescent  :: Int
  , lineSpans    :: [RenderedSpan]  -- For rich text
  }

data RenderedSpan = RenderedSpan
  { spanRect    :: Rect
  , spanTexture :: Texture
  , spanFont    :: Font
  , spanColor   :: Color
  }

-- Layout functions
layoutText :: FontSpec -> Text -> Int -> TextLayout
layoutRichText :: [TextSpan] -> Int -> TextLayout
measureText :: FontSpec -> Text -> (Int, Int)  -- width, height
```

### Text Rendering Optimization

```haskell
-- Text texture caching
data TextCache = TextCache
  { textTextures  :: Map TextCacheKey Texture
  , cacheSize     :: Int
  , maxCacheSize  :: Int
  }

data TextCacheKey = TextCacheKey
  { cacheText  :: Text
  , cacheFont  :: FontSpec  
  , cacheColor :: Color
  , cacheDPI   :: Float
  } deriving (Eq, Ord)

-- Glyph atlas for performance
data GlyphAtlas = GlyphAtlas
  { atlasTexture :: Texture
  , atlasGlyphs  :: Map (Font, Char) GlyphInfo
  , atlasSize    :: (Int, Int)
  }

data GlyphInfo = GlyphInfo
  { glyphRect    :: Rect      -- Position in atlas
  , glyphMetrics :: GlyphMetrics
  }

data GlyphMetrics = GlyphMetrics
  { metricsAdvance :: Int     -- Horizontal advance
  , metricsBearing :: (Int, Int)  -- Bearing X, Y
  , metricsSize    :: (Int, Int)  -- Width, height
  }

-- High-performance text rendering
renderTextWithAtlas :: GlyphAtlas -> FontSpec -> Text -> Color -> Renderer -> IO ()
buildGlyphAtlas :: [Font] -> IO GlyphAtlas
```

## Internationalization Support

### Unicode and Text Processing

```haskell
-- Unicode-aware text operations
import qualified Data.Text.ICU as ICU

-- Text segmentation for proper cursor movement
data TextSegmentation = TextSegmentation
  { segmentChars     :: [CharRange]    -- Character boundaries
  , segmentWords     :: [WordRange]    -- Word boundaries  
  , segmentLines     :: [LineRange]    -- Line break opportunities
  , segmentSentences :: [SentRange]    -- Sentence boundaries
  }

type CharRange = (Int, Int)
type WordRange = (Int, Int)  
type LineRange = (Int, Int)
type SentRange = (Int, Int)

-- Unicode-aware operations
segmentText :: Text -> TextSegmentation
moveCursorByChar :: Int -> Text -> Int -> Int      -- Proper grapheme cluster movement
moveCursorByWord :: Int -> Text -> Int -> Int      -- Word boundary movement
findLineBreaks :: Text -> Int -> [Int]             -- Line breaking opportunities

-- Bidirectional text support (for Arabic, Hebrew, etc.)
data TextDirection = LTR | RTL | Auto
layoutBidiText :: Text -> TextDirection -> TextLayout
```

### Input Method Support

```haskell
-- IME (Input Method Editor) support for CJK languages
data IMEState = IMEState
  { imeActive      :: Bool
  , imeComposition :: Text          -- Composition text (e.g., pinyin)
  , imeCandidates  :: [Text]        -- Candidate completions
  , imeSelection   :: Int           -- Selected candidate
  }

-- IME events
data IMEEvent
  = IMEStartComposition
  | IMEUpdateComposition Text
  , IMEEndComposition Text
  | IMECandidatesChanged [Text]
  | IMECandidateSelected Int

-- IME-aware text input
processIMEEvent :: IMEEvent -> TextInputState -> TextInputState
```

## Performance Optimization

### Text Rendering Performance

```haskell
-- Optimization strategies:

-- 1. Texture caching with LRU eviction
data LRUCache k v = LRUCache
  { cacheMap   :: Map k (v, Int)  -- Value with access count
  , cacheOrder :: [k]             -- LRU order
  , cacheSize  :: Int
  , maxSize    :: Int
  }

-- 2. Batched text rendering
batchTextRenders :: [TextRender] -> Renderer -> IO ()

-- 3. Glyph atlas with automatic packing
packGlyphsIntoAtlas :: [Glyph] -> (Int, Int) -> Either PackError GlyphAtlas

-- 4. Incremental text layout (for editors)
data IncrementalLayout = IncrementalLayout
  { layoutValid :: [Bool]          -- Which lines are still valid
  , layoutCache :: Vector TextLine -- Cached line layouts
  }

updateIncrementalLayout :: Text -> Range -> IncrementalLayout -> IncrementalLayout

-- 5. Text shaping for complex scripts
shapeText :: Font -> Text -> [ShapedGlyph]
data ShapedGlyph = ShapedGlyph
  { glyphId     :: Word32
  , glyphPos    :: (Float, Float)  -- Precise positioning
  , glyphAdvance :: (Float, Float)
  }
```

## Clipboard Integration

```haskell
-- Cross-platform clipboard support
data Clipboard = Clipboard
  { clipboardText    :: IO (Maybe Text)
  , clipboardSetText :: Text -> IO ()
  , clipboardHasText :: IO Bool
  }

-- Clipboard operations
copy :: Text -> Effect msg
paste :: (Text -> msg) -> Effect msg
cut :: Text -> (Text -> msg) -> Effect msg

-- Usage in text input
handleKeypress :: KeyCode -> Modifiers -> TextInputState -> (TextInputState, [Effect msg])
handleKeypress KeyC (Modifiers _ True _ _) state = 
  let selectedText = getSelection state
  in (state, [copy selectedText])
```

## Integration with UIH-Elm

### Text Widgets in the Framework

```haskell
-- Text widgets integrate seamlessly with UIH-Elm architecture
data AppModel = AppModel
  { userName :: Text
  , userBio  :: Text
  , editing  :: Bool
  }

data AppMsg
  = UpdateName Text
  | UpdateBio Text
  | StartEditing
  | StopEditing

appView :: AppModel -> Widget AppMsg  
appView model = 
  column
    [ text "User Profile" titleStyle
    , if editing model
        then column
          [ textInput (userName model) UpdateName
          , textArea (userBio model) UpdateBio
          , button "Save" StopEditing
          ]
        else column  
          [ text (userName model) defaultStyle
          , text (userBio model) defaultStyle
          , button "Edit" StartEditing
          ]
    ] defaultStyle

-- Rich text example
aboutView :: Widget msg
aboutView = richText
  [ TextSpan "Welcome to " Nothing Nothing Nothing Nothing
  , TextSpan "UIH-Elm" (Just boldFont) (Just blueColor) Nothing Nothing
  , TextSpan ", the " Nothing Nothing Nothing Nothing
  , TextSpan "functional GUI framework" (Just italicFont) Nothing Nothing Nothing
  , TextSpan " for Haskell!" Nothing Nothing Nothing Nothing
  ]
```

## Implementation Phases

### Phase 1: Basic Text (Week 1)
- [x] Plain text widgets with existing font system
- [x] Basic text input with cursor
- [x] Simple text styling (font, size, color)

### Phase 2: Rich Text (Week 2)  
- [ ] Multi-styled text spans
- [ ] Text measurement and layout
- [ ] Text selection and clipboard
- [ ] Text wrapping and alignment

### Phase 3: Advanced Input (Week 3)
- [ ] Multi-line text areas
- [ ] Input validation and filtering
- [ ] Undo/redo for text editing
- [ ] Keyboard shortcuts (Ctrl+A, Ctrl+C, etc.)

### Phase 4: Performance (Week 4)
- [ ] Text texture caching
- [ ] Glyph atlas optimization
- [ ] Incremental layout updates
- [ ] Performance benchmarking

### Phase 5: Internationalization (Week 5)
- [ ] Unicode-aware text operations
- [ ] Bidirectional text support
- [ ] IME integration for CJK input
- [ ] System font discovery

## Migration from Current Code

Your existing text infrastructure provides an excellent foundation:

```haskell
-- Reuse existing font loading logic
migrateFont :: UI.Hatto.SDL.Fonts.SDLState -> FontRegistry
migrateFont oldState = FontRegistry
  { registeredFamilies = convertFontMap (loadedFonts oldState)
  , loadedFonts = loadedFonts oldState
  , defaultFont = FontSpec "Roboto-Light" 32 Regular
  , fallbackFonts = ["Roboto-Light"]
  }

-- Adapt existing text input handling
migrateTextInput :: UI.PicoUI.Raw.Events.Event -> TextInputEvent
migrateTextInput (ETextInput _ txt) = TextEntered txt
-- ... other event conversions
```

## Conclusion

UIH-Elm will provide comprehensive text support that builds on your existing solid foundation while adding modern features like rich text, advanced input handling, and performance optimizations. The type-safe, functional approach will make text handling both powerful and predictable.

Key advantages:
- **Performance**: Texture caching + glyph atlas for speed
- **Features**: Rich text, input validation, clipboard integration
- **International**: Full Unicode and IME support  
- **Type Safety**: Compile-time guarantees for text operations
- **Migration**: Smooth transition from current codebase

This text system will enable building sophisticated text-heavy applications like code editors, document processors, and rich content applications, all while maintaining the functional purity and performance of UIH-Elm.