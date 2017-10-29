{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, GADTs #-}
module Screen.TestUI where

import Color
import Linear

import Screen.RawWidgets

-- given width and height of the main window return a widget
mainMenu :: Int -> Int -> Widget
mainMenu w h = WPanel $ Panel {
    box = Box {width = w, height = 60, color = mdGrey 100,
        globalX = 0, globalY = 0, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Nothing,
    borderBottom = Just $ Border {style = Solid, width = 1, color = mdGrey 700},
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

mFile = WTextLabel $ TextLabel {
    textBox = Box {width = 100, height = 40, color = mdBlack,
        globalX = 10, globalY = 16, parentX = 0, parentY = 0},
    text = "File",
    fontSize = 14
}

secondMenu :: Int -> Int -> Widget
secondMenu w h = WPanel $ Panel {
    box = Box {width = w, height = 260, color = mdBlue 500,
        globalX = 0, globalY = 61, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Nothing,
    borderBottom = Just $ Border {style = Solid, width = 4, color = mdGrey 300},
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

leftCol :: Int -> Int -> Widget
leftCol w h = WPanel $ Panel {
    box = Box {width = 500, height = h - 325, color = mdWhite,
        globalX = 0, globalY = 325, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Just $ Border {style = Solid, width = 2, color = mdGrey 300},
    borderBottom = Nothing,
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

fullUI :: Int -> Int -> [Widget]
fullUI w h = [mainMenu w h, secondMenu w h, leftCol w h, mFile]
