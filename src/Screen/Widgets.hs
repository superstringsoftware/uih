{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.Widgets where

import Color
import Linear
import Data.Text
import Data.Monoid
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import Linear -- geometry primitives

import SDL as SDL hiding (get)

import UIH.SDL.SDLIO
import UIH.SDL.Fonts
import UIH.SDL.Rendering


-- represents collider shapes - for mouse events and eventually collisions
data Collider = ColliderRect (V4 Int) | ColliderCircle (V3 Int)

-- styles for basic(est) elements, more complex components are built from them
data BoxStyle = BoxStyle {
    size :: V4 Int -- can be a bounding box, can be something else - depending on the settings regarding layout
  , color :: RGBA
}

data TextStyle = TextStyle {
    fontName :: Text
  , fontSize :: Int
  , fontColor :: RGBA
}

-- COMPONENTS -------------
-- the idea is: components represent UI, they get converted to textures at render time
data SDLComponent = SDLComponent {
    id          :: Text
  , name        :: Text
  , pos         :: V2 Int -- position relative to parent
  , collider    :: Maybe Collider
  , renderSDL   :: SDLIO (Maybe Texture) 
  -- function that renders the component. this will probably be redesigned in the future.
  -- but for now can do stuff like render = renderText txt txtStyle thanks to PAP and currying, 
  -- so this is sort of a virtual function
  -- OK THIS IS DEFINITELY BAD DESIGN. Type Synonim Families??? So that it's extensible?
}

-- now, "constructor functions"
textLabel txt txtStyle tname tid = SDLComponent {
    name = tname,
    id = tid,
    collider = Nothing,
    renderSDL = do
                    ren <- gets mainRenderer
                    fnt <- getDefaultFont -- need to change to the font lookup!
                    let clr = rgbaToV4Color (fontColor txtStyle)
                    maybe (liftIO (print "Couldn't find font when rendering TextLabel") >> return Nothing) (textToMaybeTexture txt ren clr) fnt
                    
}

