-- 5.1.1 セクション
-- 関数定義にCharの引数の定義があるのに仮引数はなしにすることによって機能させている このスタイルをHaskellだとポイントフリースタイルと呼ぶとのこと
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- 5.2 高階関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 5.3.3. mapとfilter
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- 5.5.1 左畳み込み
-- ポイントフリースタイルによる左畳み込みでのsum実装
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- 5.5.2 右畳み込み
-- 左畳み込みだとリストから新しいリストを構築するときに ++ 使わないといけなくと遅いので、このケースは右畳み込みを使うことが多い
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- 5.5.6 無限リストを畳み込む
-- fldrで実装しているので無限リストも処理可能 (Falseがあったら打ち切る)
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
