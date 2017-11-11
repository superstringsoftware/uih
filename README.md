## Some conceptual ideas:

We need 3 representations for the UI:

1) Declarative set up
2) Internal representation that has explicit calculated dimensions, styles etc + keeps the current state, getting it from reactive vars and the like -- these 2 need to be abstract and general enough to allow different rendering engines.
Layering logic needs to be handled here as well - renderer should just draw without thinking, in the right order.
3) Low-level library specific efficient rendering primitives - this can be SDL, OpenGL, Vulkan etc...

For events handling, when registering a handler with a widget / event, need to register a *region* (x,y,w,h) with a low-level lib specific event dispatcher, so that it can map primitive events and route them to a respective high-level handler

## Before building --

export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig/

- to find sdl2 linux libs!!!

On mac:
brew install sdl2
brew install sdl2_ttf
brew install sdl2_gfx

Libs are normally linked by brew into something like -L/usr/local/lib -lSDL2 - which should be available on the PATH by default so GHC should just compile it.

sdl2-gfx - works, not sure of the performance
checkout Cairo? -- https://hackage.haskell.org/package/sdl2-cairo
https://hackage.haskell.org/package/cairo-0.13.3.1


## Macos font locations:

"User"	~/Library/Fonts/
Each user has complete control over the fonts installed in their Home. These fonts are available to that user when he or she is logged in to the computer. Fonts installed here are not available to all users of the computer.
"Local"	/Library/Fonts/	Any local user of the computer can use fonts installed in this folder. Mac OS X does not require these additional fonts for system operation. An admin user can modify the contents of this folder. This is the recommended location for fonts that are shared among applications.
"Network"	/Network/Library/Fonts/	The Network folder is for fonts shared among all users of a local area network. This feature is normally used on network file servers, under the control of a network administrator.
"System"	/System/Library/Fonts/	Mac OS X requires fonts in this folder for system use and displays. They should not be manually altered or removed.
"Classic"	/System Folder/Fonts/	This folder contains fonts used by the Classic environment (Mac OS X v10.4 or earlier only). If more than one Mac OS 9.1 System Folder is present, only fonts in the System Folder selected in the Classic pane of System Preferences are used. Classic applications can access only these fonts, not those stored elsewhere. Conversely, Mac OS X applications can use these fonts, even when the Classic environment is not active.

- Google Fonts lets download fonts in ttf format
