## New rendering engine approach

So, we forget FRP for now - that was a useful exercise to learn how it works, but the learning curve for people is too high. We will use as little abstraction on top of SDL as possible to run the rendering engine; for UI definition language - will use something similar to QML.

Our state monad will have to have all loaded fonts, tree of the UI etc.

### React-like approach

Have a "render" function of sorts that "renders" our Component via translating it to low-level available widgets (TextLabel, Box, MultilineText).

So, we will have:

- Low-level *Widgets* that are rendered to SDL Texture (but eventually we may want to use different backends)
- High-level *Components* that "render" similar to react components into sets of low-level widgets. Then we can add different render functions for different backends, while keeping high-level UI definition as is.
- Eventually, we may use something like HTML for these Components and gradually move towards full browser rendering engine in haskell :)

### React logic:
Lift the state up. "State" is reactive - when it changes, we need to re-render (in a smart way eventually), "props" are read-only classic functional values, so we can render them once (into Texture) and then re-render each time as needed (also room for optimizations).

Let's take the js react tutorial around tic-tac-toe and think how it can be implemented in Haskell. Starting with the `Square` component, that is "dumb" and returns a value given in `props` from the parent component (`Board`). It handles events however, but passes their handling also to the parent component.

```js
class Square extends React.Component {
  render() {
    return (
      <button
        className="square"
        onClick={() => this.props.onClick()}
      >
        {this.props.value}
      </button>
    );
  }
}
```
In haskell, we can have something like:
```haskell
class Component props where
    render :: props -> Widgets -- compilation to low-level widgets function

data TTTValues = X | O | None deriving Show

data Square = Square { value :: TTTValues }

instance Component Square where
    render Square{..} = mkButton {
        classes = ["square"]
      , onClick = undefined -- ok, event handling is out of scope for now, need to rethink
      , text = show value
    }
```

Pretty much 1 to 1 translation. Now, let's look at the `Board` component, that stores the state:

```js
class Board extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      squares: Array(9).fill(null),
    };
  }

   renderSquare(i) {
    return (
      <Square
        value={this.state.squares[i]}
        onClick={() => this.handleClick(i)}
      />
    );
  }
```

First, start with constructor and a renderSquare function (that is used in the render call):
```haskell
data Board = Board {
    squares :: ReactiveVar [TTTValues] -- this is stateful; how do we handle it?
}
emptyBoard = Board { squares = put $ replicate 9 None }

renderSquare :: Board -> Int -> Square
renderSquare Board{..} n = Square { value = squares ! n}
```

Now, the rendering function for the Board:
```js
  render() {
    const status = 'Next player: X';

    return (
      <div>
        <div className="status">{status}</div>
        <div className="board-row">
          {this.renderSquare(0)}{this.renderSquare(1)}{this.renderSquare(2)}
        </div>
        <div className="board-row">
          {this.renderSquare(3)}{this.renderSquare(4)}{this.renderSquare(5)}
        </div>
        <div className="board-row">
          {this.renderSquare(6)}{this.renderSquare(7)}{this.renderSquare(8)}
        </div>
      </div>
    );
  }
```

Also just a straightforward translation to low-level widgets:
```haskell
instance Component Board where
    render brd@Board{..} = H.div attrs 
        [H.div attrs{classes = ["status"]},
         H.div attrs{classes = ["board-row"]}
            [renderSquare brd 0, renderSquare brd 1, renderSquare brd 2],
         H.div attrs{classes = ["board-row"]}
            [renderSquare brd 3, renderSquare brd 4, renderSquare brd 5],
         H.div attrs{classes = ["board-row"]}
            [renderSquare brd 6, renderSquare brd 7, renderSquare brd 8] ]
```
Now, obviously we can optimize it to a map function instead of this monstrousity.

Event handling is the most tricky part. React simply has this function and passes it to `Square` which then calls it every time a button is clicked. We can't do it in haskell if we want to remain functional:
```js
  handleClick(i) {
    const squares = this.state.squares.slice();
    squares[i] = 'X';
    this.setState({squares: squares});
  }
```

What are our options? We can pass some Action in some state-like Monad that would modify this state accordingly, but then we'd need to read the modified state and update our component with it somehow - behind the scenes, wherever we are storing our components (Tree? Hashtable?). To do it, we need to somehow pass IDs behind the scene properly - question is, how? Because the alternative is mutable references, which we want to avoid as much as possible.

Other approach:

```haskell
handleClick 0 = modifyState [1,0,0] b
handleClick 1 = modifyState [0,1,0] b
handleClick 2 = modifyState [0,0,1] b

renderSquare i = H.div attrs {
        classes="square", eventHandler = \e -> filter (e == OnClick) (handleClick i)
    }

renderBoard = map renderSquare state
```

### Straightforward IORefs approach

```haskell
data TTTValues = X | O | None deriving Show

class Renderable s where
  render :: s -> Widget

-- pure rendering functions first
instance Renderable TTTValues where 
  render X = H.box attrs{classes=["cell"]} "X"
  render O = H.box attrs{classes=["cell"]} "O"
  render None = H.box attrs{classes=["cell"]} ""

-- here, 'c' has to be Renderable and renderEventful registers it with the monad with event processing function
renderBoard :: IORef [TTTValues] -> FemtoUIM Widget
renderBoard b = readIORef b >>= \c -> H.div [
  H.h1 "Board:",
  renderEventful (onClick $ modifyIORef [X, O, O]) c ! 0,
  renderEventful (onClick $ modifyIORef [O, X, O]) c ! 1,
  renderEventful (onClick $ modifyIORef [O, O, X]) c ! 2,
] 

-- here, we create a new Board and set up its rendering rules in the monad
setupGUI = renderBoard $ newIORef [None,None,None]

```

Another example that keeps its' state inside - value box and a button. State for the value is in our Calculator component, while a "+" button keeps its state internally - and we want to make it change background when hovering.

  Calculator
| 5 |   | + |

```haskell
eh = onHover $ setClass "hovering" -- setClass is basically modifyIORef under the hood
btnPlus :: IORef ButtonData -> FemtoUIM Widget
btnPlus ib = renderEventful (onHover $ setClass ib "hovering") (H.button attrs{classes="small"} "+")
```
