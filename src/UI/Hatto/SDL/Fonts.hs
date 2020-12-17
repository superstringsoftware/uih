{-# LANGUAGE OverloadedStrings #-}

module UI.Hatto.SDL.Fonts 
where

import SDL hiding (get)
import SDL.Font

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Exception ( try )

import Data.Text

import UI.Hatto.Widgets

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
safeLoadFont :: MonadIO m => MutState SDLState -> String -> Int -> m (Maybe Font)
safeLoadFont stm path size = do
    st <- readMutState stm
    let (V2 x y) = scaleXY st
    let size' = if autoScale st then round ( fromIntegral size * (x + y) / 2) else size
    r <- liftIO $ try $ load path size'
    either (\e -> liftIO $ print (e::SDLException) >> return Nothing)
                (return • Just) r

-- handles scaling of font related sizes used in rendering etc - need this for high dpi stuff
scaleFontSizeDown :: MonadIO m => MutState SDLState -> Int -> m Int
scaleFontSizeDown stm size = do
    st <- readMutState stm
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

getDefaultFont :: MonadIO m => MutState SDLState -> m Font
getDefaultFont stm = do 
    st <- readMutState stm
    let fntm = Map.lookup defaultFontKey (loadedFonts st)
    maybe (fail "Could not find default font, impossible to continue!")
          pure fntm

getFont :: MonadIO m => MutState SDLState -> (Text, Int) -> m (Maybe Font)
getFont stm txt = do 
    st <- readMutState stm
    pure $ Map.lookup txt (loadedFonts st)
          

getFontOrDefault :: MonadIO m => MutState SDLState -> (Text, Int) -> m Font
getFontOrDefault stm txt = do
    fntm <- getFont stm txt
    maybe (getDefaultFont stm) pure fntm

initFonts :: MonadIO m => MutState SDLState -> m ()
initFonts stm = do
    st <- readMutState stm
    fnt <- initDefaultFont
    let fonts = Map.insert defaultFontKey fnt (loadedFonts st)
    fd_mns <- isMonospace fnt
    fd_fml <- familyName fnt
    fd_sn <- styleName fnt
    fd_ls <- lineSkip fnt
    let fd = SDLFontData fd_mns fd_fml fd_sn fd_ls
    liftIO $ putStrLn $ "Loaded font:\n" ++ show fd
    updateMutState stm (const st {loadedFonts = fonts})
    where initDefaultFont = do   
            mfont <- SDL.Font.initialize >> safeLoadFont stm defaultFontPath (snd defaultFontKey)
            maybe (fail "Could not initialize TTF fonts!") pure mfont

destroyFonts :: MonadIO m => MutState SDLState -> m ()
destroyFonts stm = readMutState stm >>= \st -> mapM_ (liftIO . free) (loadedFonts st)