1. Fill in the blank for this tax computing program, assuming 8% tax:

```haskell
totalAmount :: Float -> Float
totalAmount subtotal = subtotal * tax
    where ...
```

2. Use the `let..in` pattern to write a function called `show3DPoint`, which
   accepts `(x, y, z)` and prints a string representation.

```
show3DPoint :: Num a => (a, b, c) -> [Char]
```


3. Encode these logic statements using `if..then` expressions, using no logical
   operators or other patterns:

```
eligible :: String -> Int -> Bool
eligible name age = ...  -- Your implementation here
```

If you are over 18 and your name is "John", then you're eligible.
If you are over 18 and your name is "Tom", then you're eligible.
If your name is "Sarah", then you're eligible.
