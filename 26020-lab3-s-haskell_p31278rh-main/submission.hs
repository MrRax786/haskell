
-- Quadtree Definition
data Quadtree =  BlackCell | WhiteCell | Node Quadtree Quadtree Quadtree Quadtree
    deriving (Eq, Show)

-- Function for Black Cell creation
allBlack :: Int -> Quadtree
allBlack _ = BlackCell

-- Function for White Cell creation
allWhite :: Int -> Quadtree
allWhite _ = WhiteCell

-- Clockwise Function
clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Node a d c b


-- Anticlockwise Function
anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
anticlockwise a b c d = Node d b c a








-- data Quadtree = Cell Bool 
--     | Node Quadtree Quadtree Quadtree Quadtree

-- instance Eq Quadtree where 
--     (Cell c1) == (Cell c2) = c1 == c2
--     (Node q1 q2 q3 q4) == (Node q1' q2' q3' q4') = q1 == q1' && q2 == q2' && q3 == q3' && q4 == q4'
--     _ ==_= False

-- instance Show Quadtree where
--     show(Cell True) = " "
--     show (Cell False) = " "
--     show(Node q1 q2 q3 q4) = "/n" ++ show q1 ++ show q2 ++ show q3 ++ show q4

-- -- Function for Black Cell creation
-- allBlack :: Int -> Quadtree
-- allBlack _ = Cell True

-- -- Function for White Cell creation
-- allWhite :: Int -> Quadtree
-- allWhite _ = Cell False

-- -- Clockwise Function
-- clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
-- clockwise q1 q2 q3 q4 = Node q1 q2 q3 q4


-- -- Anticlockwise Function
-- anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
-- anticlockwise q1 q2 q3 q4 = Node q1 q2 q3 q4

