{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module UIH.UI.Renderable where

class Monad m => Renderable m a where
    -- what we return from *intermediary* rendering function, e.g. Texture in SDL
    type Res m a
    -- Intermediate render function, returning Res
    render :: a -> m (Res m a)
    -- Final render function, drawing to screen and returning whatever pure data we were rendering, updated if needed
    renderScreen :: a -> m ()
