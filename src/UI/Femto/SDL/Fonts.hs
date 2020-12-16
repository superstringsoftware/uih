{-# LANGUAGE OverloadedStrings #-}

module UI.Femto.SDL.Fonts where

import UI.Femto.SDL.SDLMonad

import SDL hiding (get)
import SDL.Font

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Exception ( try )

import Data.Text

import PreludeFixes

----------------------------------------------------------------------------------------------------
-- font stuff
----------------------------------------------------------------------------------------------------
defaultFontPath = "./fonts/Roboto/Roboto-Light.ttf"
defaultFontKey :: (Text, Int)
defaultFontKey = ("Roboto-Light", 32)

-- ALL FONT LOADING NEEDS TO BE DONE VIA THIS FUNCTION
-- it handles scaling for dpi etc
-- The logic is:
-- We make font size SCALE UP in high-dpi environments
-- when we render font related textures, we scale back to 0 so that they are rendered correctly
safeLoadFont :: String -> Int -> FemtoUIM (Maybe Font)
safeLoadFont path size = do
    autos <- gets autoScale
    V2 x y <- gets scaleXY
    let size' = if autos then round ( fromIntegral size * (x + y) / 2) else size
    r <- liftIO $ try $ load path size'
    either (\e -> liftIO $ print (e::SDLException) >> return Nothing)
                (return • Just) r

-- handles scaling of font related sizes used in rendering etc - need this for high dpi stuff
scaleFontSizeDown :: Int -> FemtoUIM Int
scaleFontSizeDown size = do
    V2 x y <- gets scaleXY
    autos <- gets autoScale
    let size' = if autos then round ( fromIntegral size / ((x + y) / 2)) else size
    return size'
          
data SDLFontData = SDLFontData {
    fntIsMonospace :: Bool,
    fntFamilyName :: Maybe Text,
    fntStyleName :: Maybe Text,
    fntLineSkip :: !Int
} deriving (Eq, Show)

getDefaultFont :: FemtoUIM Font
getDefaultFont = do 
    st <- get 
    let fntm = Map.lookup defaultFontKey (loadedFonts st)
    maybe (fail "Could not find default font, impossible to continue!")
          pure fntm

getFont :: (Text, Int) -> FemtoUIM (Maybe Font)
getFont txt = Map.lookup txt <$> gets loadedFonts
          

getFontOrDefault :: (Text, Int) -> FemtoUIM Font
getFontOrDefault txt = do
    st <- get 
    let fntm = Map.lookup txt (loadedFonts st)
    maybe getDefaultFont
          pure fntm

initFonts :: FemtoUIM ()
initFonts = do
    fnt <- initDefaultFont
    st <- get
    let fonts = Map.insert defaultFontKey fnt (loadedFonts st)
    fd_mns <- isMonospace fnt
    fd_fml <- familyName fnt
    fd_sn <- styleName fnt
    fd_ls <- lineSkip fnt
    let fd = SDLFontData fd_mns fd_fml fd_sn fd_ls
    liftIO $ putStrLn $ "Loaded font:\n" ++ show fd
    put st {loadedFonts = fonts}
    where initDefaultFont = do   
            mfont <- SDL.Font.initialize >> safeLoadFont defaultFontPath (snd defaultFontKey)
            maybe (fail "Could not initialize TTF fonts!") pure mfont

destroyFonts :: FemtoUIM ()
destroyFonts = get >>= \st -> mapM_ (liftIO . free) (loadedFonts st)