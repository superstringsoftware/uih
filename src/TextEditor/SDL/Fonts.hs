{-# LANGUAGE OverloadedStrings #-}

module TextEditor.SDL.Fonts
where

import SDL hiding (get)
import SDL.Font

import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Exception ( try )

import Data.Text

import Control.Monad.MRWS
import TextEditor.SDL.SDLMonad


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
safeLoadFont :: MonadIO m => String -> Int -> SDLUIT m (Maybe Font)
safeLoadFont path size = do
    st <- get
    let (V2 x y) = scaleXY st
    let size' = if autoScale st then round ( fromIntegral size * (x + y) / 2) else size
    r <- liftIO $ try $ load path size'
    either (\e -> liftIO $ print (e::SDLException) >> return Nothing)
                (return . Just) r

-- handles scaling of font related sizes used in rendering etc - need this for high dpi stuff
scaleFontSizeDown :: MonadIO m => Int -> SDLUIT m Int
scaleFontSizeDown size = do
    st <- get
    let (V2 x y) = scaleXY st
    let autos = autoScale st
    let size' = if autos then round ( fromIntegral size / ((x + y) / 2)) else size
    return size'

data SDLFontData = SDLFontData {
    fntIsMonospace :: Bool,
    fntFamilyName :: Maybe Text,
    fntStyleName :: Maybe Text,
    fntLineSkip :: !Int
} deriving (Eq, Show)

getDefaultFont :: (MonadIO m, MonadFail m) => SDLUIT m Font
getDefaultFont = do
    st <- get
    let fntm = Map.lookup defaultFontKey (loadedFonts st)
    maybe (fail "Could not find default font, impossible to continue!")
          pure fntm

getFont :: (Monad m, MonadIO m) => (Text, Int) -> SDLUIT m (Maybe Font)
getFont txt = Map.lookup txt . loadedFonts <$> get

getFontOrDefault :: (Monad m, MonadIO m, MonadFail m) => (Text, Int) -> SDLUIT m Font
getFontOrDefault txt = do
    fntm <- getFont txt
    maybe getDefaultFont pure fntm

initFonts :: (Monad m, MonadIO m, MonadFail m) => SDLUIT m ()
initFonts = do
    st <- get
    fnt <- initDefaultFont
    let fonts = Map.insert defaultFontKey fnt (loadedFonts st)
    fd_mns <- isMonospace fnt
    fd_fml <- familyName fnt
    fd_sn <- styleName fnt
    fd_ls <- lineSkip fnt
    let fd = SDLFontData fd_mns fd_fml fd_sn fd_ls
    liftIO $ putStrLn $ "Loaded font:\n" ++ show fd
    modify (const st {loadedFonts = fonts})
    where initDefaultFont = do
            mfont <- SDL.Font.initialize >> safeLoadFont defaultFontPath (snd defaultFontKey)
            maybe (fail "Could not initialize TTF fonts!") pure mfont

destroyFonts :: MonadIO m => SDLUIT m ()
destroyFonts = get >>= \st -> mapM_ (liftIO . free) (loadedFonts st)