{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , DataKinds
    , TypeApplications
    , FlexibleContexts
    , RankNTypes
    , BlockArguments
     #-}

module UIH.UI.Widgets where

-- Combining AbstractWidgets and Widgets together so that it all renders as needed

-- import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text as T
import Linear
import Color

import UIH.UI.AbstractWidgets
import UIH.UI.BasicWidgets
import UIH.UI.Handlers