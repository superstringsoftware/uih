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

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts  #-}
module SDL.InputText where

import SDL.Bindings
import CSS.Box
import Color

import Data.Text
import Screen.Text as ST

import SDL.Font
import SDL.Fonts

import SDL.SDLIO
import SDL.Video.Renderer

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict 
import Control.Monad (foldM_, foldM)

import Linear
import Foreign.C.Types (CInt)


testText = DecoratedText
    {
        text = "Hello World!\n\t\tHow about we make it multiline?"
      , fontName = "Roboto-Light"
      , fontSize = 16
      , textStyles = [ST.Underline]
      , color = mdGrey 700
    }

testBox = TextBox 
    {
        topLeft = V2 40 400 
      , dimensions = V2 800 600
      , textLines = [ "-- a line of text - this means, the text itself has to be pre-processed for *newlines* and split into these lines"
                    , "-- also, TABS MUST be converted to SPACES before going here"
                    , "-- this means, in an editor - we'll have source text and then decorated thingy split into lines etc - not very space efficient?"
                    , "-- can we treat it differently? E.g., have text split into lines and then STYLE DEMARKATIONS if they need to be applied at a certain place?"
                    , "-- This way, we can have a whole file as Vector Text (to be able to index) and style with index: line num, char num"
                    ]
      , cursorPosition = V2 0 0 
    }

-- render one line of text with the default font
renderSimpleText :: RGBA -> Text -> SDLIO Surface
renderSimpleText color txt = do
    (Just font) <- getDefaultFont
    blended font (rgbaToV4Color color) txt

-- function takes a DecoratedText and turns it into a Surface (why not a texture?)
text2surface :: DecoratedText -> SDLIO Surface
text2surface dt = do
    (Just font) <- getDefaultFont
    blended font (rgbaToV4Color $ color dt) (text dt)

-- helpFunc :: CInt -> Renderer -> CInt -> Texture -> IO CInt


instance Renderable TextBox where
    render renderer tb x y = do
                tsurfs <- mapM (renderSimpleText $ mdGrey 700) (textLines tb)
                texs <- mapM (createTextureFromSurface renderer) tsurfs
                mapM_ freeSurface tsurfs
                liftIO $ foldM_ (helpFunc x renderer) y texs
                liftIO $ mapM_ destroyTexture texs
                -- we are using foldM_ instead of a for cycle in an imperative language:
                -- need to render each new texture at a NEW y coordinate, how do we increment it?
                -- we use accumulator of the foldM function as a sort of internal state to handle this!
                where helpFunc x renderer acc t = do
                                    info <- queryTexture t
                                    renderTexture x acc t renderer
                                    return (acc + (textureHeight info)) -- passing state inside foldM
                
    renderGlobal tb = do    ren <- gets mainRenderer
                            let (V2 x y) = topLeft tb 
                            render ren tb (fromIntegral x) (fromIntegral y)



instance Renderable DecoratedText where
    render renderer dt x y = do
                tsurf <- text2surface dt
                tex <- liftIO $ createTextureFromSurface renderer tsurf 
                liftIO $ renderTexture x y tex renderer
                liftIO $ freeSurface tsurf
    
    renderGlobal tl = do    ren <- gets mainRenderer
                            render ren tl 100 100