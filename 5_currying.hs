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
