import Data.Char
import Data.List
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- 6.2.3 シーザー暗号
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode offset msg = map (\c -> chr $ ord c - offset) msg

-- 6.3 key value mapping
-- e.g: Map.lookup "foo" phoneBook
phoneBook :: Map.Map String String
phoneBook =
  Map.fromList $
    [ ("foo", "555-2938"),
      ("bar", "452-2928")
    ]
