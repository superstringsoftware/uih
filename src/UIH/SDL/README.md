# Design ideas

On low level, we need to render:

* Boxes, with borders, shadows and different background options (CSS like)
* Lines - to avoid too much texture copying when we need some lines drawn inside a box (like panel with internal borders or something)
* Various text related stuff, see below

## Text handling

* Needs to have state - simply text without any highlighting etc, and representation for rendering.
* State is Sequence Text, where each line is a separate entry. Need to think about the container - need to be able to insert in the middle very quickly (if a user presses "Enter" creating a new line), so it's not vector and it's not a list either
* For rendering, each line needs to be split into pieces of different style (color etc) - so we need to do a first markup pass and then pass a Vector of lines to the renderer to show

So, a typical cycle would look like:

<some user event changing text> --> update State representation --> recalculate markup (coloring, styles etc) --> re-render

### Let's start with a simple 1-line input box.

- State: Text
- Events: typing, moving cursor
- Rendering: render the whole line to a texture, copy only visible part to the bounding box
