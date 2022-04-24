import Util

data Command
  = Down Int
  | Up Int
  | Forward Int

parseCommand :: String -> Command
parseCommand = f . split ' '
  where
    f ["forward", n] = Forward (read n)
    f ["down", n] = Down (read n)
    f ["up", n] = Up (read n)
    f _ = undefined

parse :: String -> [Command]
parse = map parseCommand . split '\n'

solve :: [Command] -> Int
solve commands = uncurry (*) $ foldl f (0, 0) commands
  where
    f (x, y) (Down n) = (x, y + n)
    f (x, y) (Up n) = (x, y - n)
    f (x, y) (Forward n) = (x + n, y)

dbg :: Show a => a -> IO a
dbg a = do
  print a
  return a

solve2 :: [Command] -> Int
solve2 commands = x * y
  where
    (aim, x, y) = foldl f (0, 0, 0) commands
    f :: (Int, Int, Int) -> Command -> (Int, Int, Int)
    f (aim, x, y) (Down n) = (aim + n, x, y)
    f (aim, x, y) (Up n) = (aim - n, x, y)
    f (aim, x, y) (Forward n) = (aim, x + n, y + aim * n)

main :: IO ()
main = interact $ show . solve2 . parse
