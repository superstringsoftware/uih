{-
Input Text control - SDL implementation.
Logic:
- Have TextVar that contains current text
- Render Box to a texture
- Render current text to a texture (if changed) - take length into account!!! (not render the whole string if too long -- scrolling?)
- Set font parameters
- Give texture to parent rendering mechanism to render when needed (?????)
- In the simple case - simply copy texture to renderer
- Handle keyboard events to update text

- how to define that this input has focus??? (to handle text input)
-}

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}
module SDL.InputText where

import SDL.Bindings
import CSS.Box
import Color
