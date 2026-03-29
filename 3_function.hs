factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

-- 3.1.3
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 3.2: ガード
max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- 3.3. where
greet :: String -> String -> String
greet firstName lastName = "Hi " ++ name ++ " !"
  where
    -- where内には関数も定義可能である
    name = firstName ++ " " ++ lastName

-- 3.4. let
-- let式はwhere節にとてもよく似ています。whereは関数の終わりで変数を束縛し、その変数はガードを含む関数全体から見えます。それに対してlet式は、どこでも変数を束縛でき、そしてlet自身も式になります。しかし、let式が作る束縛は局所的で、ガード間で共有されません。束縛を行うHaskellの他の構文と同じく、let式でもパターンマッチが使えます。
-- ＭｉｒａｎＬｉｐｏｖａｃａ. すごいHaskellたのしく学ぼう！ (p. 86). (Function). Kindle Edition.
greet' :: String -> String -> String
greet' firstName lastName =
  let name = firstName ++ " " ++ lastName
   in "Hi " ++ name ++ " !"

-- リスト内包表記内での束縛も可能 (フィルタっぽいがこの場合名前を束縛するだけになる)
doubleList :: [Int] -> [Int]
doubleList xs = [doubled | x <- xs, let doubled = x * 2]
