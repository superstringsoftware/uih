## Need to define UI declaratively

- then when there's a visual UI builder it simply generates a Haskell source file with UI

Something like:

```haskell
row $ menuFile <> menuEdit <> menuHelp
row $ col $ leftMenu <>
      col $ row $ header <>
            row $ mainContent
      <>
row $ footer      
```
How do we handle state and reactivity here?

E.g., we have a header widget that should rerender depending on some reactive variable - how do we do that?

`header reactVal`

```haskell
-- value plus a bunch of functions that do some action depending on the old value & new value
data Monad m => ReactiveVar m a = Reactive a [a -> a -> m ()]

update :: ReactiveVar m a -> a -> m ()
update = do
  return expression
```
