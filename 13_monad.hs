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
