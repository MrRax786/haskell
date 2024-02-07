----------------------- Ex1 --------------------------

-- Quadtree Definition
data Quadtree =   BlackCell | WhiteCell | Node Quadtree Quadtree Quadtree Quadtree 
    deriving (Eq, Show)

-- Function for Black Cell creation
allBlack :: Int -> Quadtree
allBlack _ = BlackCell
-- allBlack n = Node subtree subtree subtree subtree
--     where subtree = allBlack (n-1)

-- Function for White Cell creation
allWhite :: Int -> Quadtree
allWhite _ = WhiteCell
-- allWhite n = Node subtree subtree subtree subtree
--     where subtree = allWhite (n-1)
-- Clockwise Function
--clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
-- clockwise (Node a1 b1 c1 d1) (Node a2 b2 c2 d2) (Node a3 b3 c3 d3) (Node a4 b4 c4 d4) = Node (clockwise a1 a2 a3 a4) (clockwise b1 b2 b3 b4) (clockwise c1 c2 c3 c4) (clockwise d1 d2 d3 d4)
clockwise a b c d = Node a b c d 


-- Anticlockwise Function
--anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
-- anticlockwise (Node a1 b1 c1 d1) (Node a2 b2 c2 d2) (Node a3 b3 c3 d3) (Node a4 b4 c4 d4) = Node (anticlockwise a4 b4 c4 d4) (anticlockwise a3 b3 c3 d3) (anticlockwise a2 b2 c2 d2) (anticlockwise a1 b1 c1 d1)
anticlockwise a b c d = Node a d c b

-- ----------------------- Ex2 --------------------------

-- blur :: Quadtree -> Quadtree
-- blur 0 = 0
-- blur n = 