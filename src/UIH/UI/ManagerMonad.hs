module UIH.UI.ManagerMonad where

-- this is a (pure?) state monad that keeps the current UI state and handles low-level events from SDL,
-- transforming them to high-level UI events (button clicks etc)