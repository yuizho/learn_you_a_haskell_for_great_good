data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 7.3 レコード構文
type PhoneNumber = String

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

-- 7.7 再帰的なデータ構造
data List a = Empty | Cons {listhead :: a, listTail :: List a}
  deriving (Show, Read, Eq, Ord)

-- 7.7.2 二分探索木
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

-- 7.8.1. Eq型クラスの内部
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

data MyMaybe a = MyNothing | MyJust a

instance (Eq m) => Eq (MyMaybe m) where
  MyJust x == MyJust y = x == y
  MyNothing == MyNothing = True
  _ == _ = False
