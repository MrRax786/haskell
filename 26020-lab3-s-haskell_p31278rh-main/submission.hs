import System.Console.Terminfo (Color(White))
import Distribution.Compat.Graph (neighbors)
import GHC.Base (neChar, neAddr#)
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



-- Get Colour of Cell
--getColour :: Bool -> [Bool] -> Bool
--getColour isBlack neighbours =
    --let neighbourTOT = length neighbours
        --oppColourCount = occuranceCount (not isBlack) neighbours
   -- in if oppColourCount > (quot neighbourTOT 2)
        --then not isBlack
       -- else isBlack

-- -- Blur Function
-- blur :: Quadtree -> Quadtree
-- blur BlackCell = WhiteCell
-- blur WhiteCell = BlackCell
-- blur(Node a b c d) =
--     let neighbours = map blur [a,b,c,d]
--         blurredColour = getColour True neighbours
--     in if blurredColour
--         then BlackCell
--         else WhiteCell

--------------------------------------------------------------------------

-- Get Cell Colour
getCellColour :: Quadtree -> Bool
getCellColour WhiteCell = False
getCellColour BlackCell = True

-- Check if node is cell
isCell:: Quadtree -> Bool
isCell quadtree = case quadtree of
    WhiteCell _ -> True
    BlackCell _ -> True
    _ -> False

-- Count Func for Num Opposite Colour Neighbours
oppColourCount :: Bool -> [Quadtree] -> Int
oppColourCount colour neighbors = length $ filter (\x -> x /= colour) (map getCellColour neighbors)

-- Get blurred cell colour
blurredColourd :: Quadtree -> [Quadtree] -> Bool
blurredColour quadtree neighbours = let
    totNeighbours = length Neighbours
    oppColourCount' = oppColourCount (not $ getCellColour quadtree ) neighbour
    in if oppColourCount' > totNeighbours 'div' 2
        then not (getCellColour quadtree)
        else getCellColour quadtree

-- Blur Single Cell Function 
cellBlur :: Quadtree -> [Quadtree] -> Quadtree
cellBlur quadtree neighbors = if isCell quadtree
    then if oppColourCount (getCellColour quadtree) neighbors > length neighbors 'div' 2
        in if getCellColour quadtree then BlackCell else WhiteCell
    else quadtree
    else quadtree

-- Blur Quadtree
blur :: Quadtree -> Quadtree
blur quadtree = let
    nodeBlur :: Quadtree -> Quadtree
    nodeBlur (Node a b c d) = Node (blurNode a) (blurNode b) (blurNode c) (blurNode d)
    nodeBlur node = node

    cellsBlur :: Quadtree -> [Quadtree] -> Quadtree
    cellsBlur quadtree neighbors = if isCell quadtree
        then cellBlur quadtree Neighbours
        else quadtree

    treeBlurCells :: Quadtre -> Quadtree
    treeBlurCells (Node a b c d) = Node (treeBlurCells a) (treeBlurCells b) (treeBlurCells c) (treeBlurCells d)
    treeBlurCells node = node

    in treeBlurCells $ blurNode quadtree 


-- -- Single Cell Blur
-- cellBlur :: Quadtree -> [Bool] -> Quadtree
-- cellBlur (Cell isBlack) neighbours = Cell (getColour isBlack neighbours)
-- cellBlur q _ = q

-- -- Entire Quadtree Blur
-- blur :: Quadtree -> Quadtree
-- blur q = mapQuadtreeWNeighbours cellBlur q

-- -- Map Function
-- mapQuadtreeWNeighbours :: ([Bool] -> Quadtree -> Quadtree) -> Quadtree -> Quadtree
-- mapQuadtreeWNeighbours f q = mapQuadtreeWNeighbours' f q []

-- -- Helper Function
-- mapQuadtreeWNeighbours' :: ([Bool] -> Quadtree -> Quadtree) -> Quadtree -> [Bool] -> Quadtree
-- mapQuadtreeWNeighbours' _ (Cell _)_ = Cell False -- BC
-- mapQuadtreeWNeighbours' f (Node a b c d) neighbours =
--     let newNeighbours = getNeighbours (map getCellColour [a b c d])
--         newA = mapQuadtreeWNeighbours' f a newNeighbours
--         newB = mapQuadtreeWNeighbours' f b newNeighbours
--         newC = mapQuadtreeWNeighbours' f c newNeighbours
--         newD = mapQuadtreeWNeighbours' f d newNeighbours
--     in f neighbours (Node newA newB newC newD)







-- -- Neighbour Colours Function
-- getNeighbours :: [Bool] -> Bool
-- getNeighbours [a, b, c, d] = [a, b, c, d]

-- -- Occurance Count
-- occuranceCount :: Eq a => a -> [a] -> Int
-- occuranceCount x = length . filter (== x)


