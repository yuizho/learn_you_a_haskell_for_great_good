solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*" = (y * x) : ys
    foldingFunction (x : y : ys) "+" = (y + x) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction xs numberString = read numberString : xs -- 最終的な戻り値の型がDoubleなので型推論できる

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

type ReadSystem = [Section]
