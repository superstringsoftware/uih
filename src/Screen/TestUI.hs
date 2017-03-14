{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.TestUI where

import Color
import Linear

import Screen.RawWidgets

-- given width and height of the main window return a widget
mainMenu :: Int -> Int -> Panel
mainMenu w h = Panel {
    box = Box {width = w, height = 60, color = mdGrey 100,
        globalX = 0, globalY = 0, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Nothing,
    borderBottom = Just $ Border {style = Solid, width = 1, color = mdGrey 700},
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

secondMenu :: Int -> Int -> Panel
secondMenu w h = Panel {
    box = Box {width = w, height = 260, color = mdBlue 500,
        globalX = 0, globalY = 61, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Nothing,
    borderBottom = Just $ Border {style = Solid, width = 4, color = mdGrey 300},
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

leftCol :: Int -> Int -> Panel
leftCol w h = Panel {
    box = Box {width = 500, height = h - 325, color = mdWhite,
        globalX = 0, globalY = 325, parentX = 0, parentY = 0},
    shadow = Nothing,
    borderTop = Nothing,
    borderRight = Just $ Border {style = Solid, width = 2, color = mdGrey 300},
    borderBottom = Nothing,
    borderLeft = Nothing,
    padding = V4 0 0 0 0
}

fullUI :: Int -> Int -> [Panel]
fullUI w h = [mainMenu w h, secondMenu w h, leftCol w h]
