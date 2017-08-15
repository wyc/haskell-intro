1. Using function pattern matching, write the function `lottery` that returns
   `True` if given the lucky lottery number of 5, and `False` otherwise.

```haskell
lottery :: Num a => a -> Bool
```

2. Write the function `multiLottery` that returns `True` if a number matches
   either 5, 7, or 9, and `False` otherwise.

```haskell
multiLottery :: Num a => a -> Bool
```

3. Re-implement `multiLottery` using guards.


4. Re-implement `multiLottery` using `case..of`.

5. Using any pattern matching technique you want, implement `geoLottery`, which returns `True` if the coordinates match `(1337, 7331)` and `False` otherwise.

```haskell
geoLottery :: Num a => (a, a) -> Bool
```

6. Implement `multiGeoLottery`, which accepts a list of winning coordinates and
   a single coordinate. It returns `True` if the single coordinate is within
   the list of winning coordinates and `False` otherwise.

```haskell
multiGeoLottery :: Num a => [(a, a)] -> (a, a) -> Bool
```

7. Use `multiGeoLottery` to implement `myGeoLottery`, which has an arbitrary
   list of winning coordinates already applied. It should accept a single
   coordinate and return `True` upon match, `False` otherwise.

```haskell
myGeoLottery :: Num a => (a, a) -> Bool
```
