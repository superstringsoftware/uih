## New rendering engine approach

So, we forget FRP for now - that was a useful exercise to learn how it works, but the learning curve for people is too high. We will use as little abstraction on top of SDL as possible to run the rendering engine; for UI definition language - will use something similar to QML.

Our state monad will have to have all loaded fonts, tree of the UI etc.