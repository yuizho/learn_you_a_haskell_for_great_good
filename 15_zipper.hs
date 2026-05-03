data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

data Direction = L | R deriving (Show)

type Directions = [Direction]

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _) = Nothing

x -: f = f x

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- (freeTree, []) >>= goLeft >>= goRight >>= goRight >>= goRight
-- do
--    x1 <- goLeft  (freeTree, [])
--    x2 <- goRight x1
--    x3 <- goRight x2
--    goRight x3
