
-- Quadtree Definition
data Quadtree = Cell Bool | Node Quadtree Quadtree Quadtree Quadtree
    deriving (Eq, Show)

-- Function for Black Cell creation
allBlack :: Int -> Quadtree
allBlack _ = Cell True

-- Function for White Cell creation
allWhite :: Int -> Quadtree
allWhite _ = Cell False

-- Clockwise Function
clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Node a b c d


-- Anticlockwise Function
anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
anticlockwise a b c d = Node d c b a



