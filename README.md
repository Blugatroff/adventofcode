# Advent of Code in Haskell
My solutions for **AOC 2021**, **AOC 2022** and **AOC 2024**!

I'm using this to learn Haskell and purely functional programming in general.
Because of that i always try to stick to the immutable solutions before falling back to using the [**ST**](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) or [**IO**](https://hackage.haskell.org/package/base-4.17.0.0/docs/System-IO.html#g:1) Monad.

## Progress
- 2021: **`1..22`**
- 2022: **`1..25`**
- 2024: **`1..11`**

## How to Run

```sh
cabal run aoc -- 2022 17 two
```

## Architecture
The solution to every day is in its own module and exports two functions, one for either part of the problem.

Both functions always have the same signature.
```haskell
partOne :: String -> Either String String
partTwo :: String -> Either String String
```
See [*2022/Day1.hs*](https://github.com/Blugatroff/adventofcode/blob/main/src/Year2022/Day1.hs) for an example of this.

The [Main](https://github.com/Blugatroff/adventofcode/blob/main/src/Main.hs) module then just has to pick the right function from the list of days, depending on the CLI arguments, and apply it to the input.


## Static musl executable, because it's neat!
```sh
docker build -t aoc . && docker run --rm -v $(pwd):/app aoc
```
