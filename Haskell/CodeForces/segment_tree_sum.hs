import Data.Monoid 
import Data.Semigroup (Min(Min))

-- Btree
data Btree x = Empty | Node x (Btree x) (Btree x) deriving Show

split2 :: Monoid x => [Btree x] -> [(Btree x, Btree x)]
split2 l = go [] l''
    where 
        l' = reverse l
        l'' = if even $ length l' then l' else Node mempty Empty Empty : l'
        go acc [] = acc
        go acc [x] = (x, Node mempty Empty Empty) : acc
        go acc (x:y:xs) = go ((y, x) : acc) xs

build :: Monoid x => [x] -> Btree x
build l = go l''
    where
        l' = if even $ length l then l else l ++ [mempty]
        l'' = [Node x Empty Empty | x <- l'] 
        go [x] = x
        go l''' = go $ [Node (x <> y) n1 n2 | (n1@(Node x _ _), n2@(Node y _ _)) <- split2 l''']

-- sg

data ST x = ST Int (Btree x) deriving Show

buildST :: Monoid x => [x] -> ST x
buildST l = ST size $ build l
    where
        size = go 1 $ length l
        go c t 
            | c < t = go (2 * c) t
            | otherwise = c

query :: Monoid x => ST x -> Int -> Int -> x
query st@(ST size tree) l r = go tree l r 0 size
    where
        go Empty _ _ _ _ = mempty
        go n@(Node v ltree rtree) l r cl cr 
            | l >= cr || cl >= r = mempty
            | cl >= l && cr <= r = v
            | otherwise = go ltree l r cl m <> go rtree l r m cr
                where
                    m = div (cl + cr) 2

update :: Monoid x => ST x  -> Int -> x -> ST x
update st@(ST size tree) at value = ST size $ go tree at value 0 size
    where
        go Empty _ _ _ _ = error "Empty node is not a possible case for update"
        go tree@(Node _ ltree rtree) i v l r 
            | l + 1 == r = Node v Empty Empty
            | otherwise = Node (vl <> v2) ltree' rtree'
                where
                    m = div (l + r) 2
                    ltree'@(Node vl _ _) = if i < m then go ltree i v l m else ltree
                    rtree'@(Node v2 _ _) = if i < m then rtree else go rtree i v m r

solve :: [Int] -> [String] -> [Int]
solve l queries = map (\(Min x) -> x) $ snd $ foldl f (buildST l', []) queries
    where 
        l' = [Min x | x <- l]
        f acc q = (st, r ++ snd acc)
            where
                [t, x, y] = [read s::Int | s <- words q]
                (st, r) = go (fst acc) t x y 
        go st t x y 
            | t == 1 = (update st x (Min y), [])
            | otherwise = (st, [query st x y])

main :: IO ()
main = do
    userInput <- getContents
    let (nm:lst:q) = lines userInput
    mapM_ print $ reverse $ solve (map read (words lst) :: [Int]) q
