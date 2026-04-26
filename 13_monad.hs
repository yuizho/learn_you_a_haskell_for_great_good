import Control.Monad

-- 13.2
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- 13.4.1
type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- モナドにしたことで前の結果を取り出して次にも適用できる (文脈をパスできる)
-- return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- バナナを使うといきなりピエールはバランスを崩す
-- return (0, 0) >>= landRight 2 >>= banana >>= landRight 2

-- (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2
x -: f = f x

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  -- <-で変数束縛せずにモナド使うと無視したいモナドのあとに >> をつけるのとおなじになる
  -- Nothing
  second <- landRight 2 first
  landLeft 1 second

-- 13.5.1
maySue :: Maybe Bool
maySue = do
  x <- Just 9
  Just (x > 8)

-- 13.5.3
justStr :: String -> Maybe Char
justStr s = do
  -- 空文字とか来るとNothingがかえる
  (x : xs) <- Just s
  return x

-- 13.6
listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
    ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
