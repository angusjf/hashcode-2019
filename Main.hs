import Data.List

type Tag = String

data Image = Image
   { index :: Int
   , horizontal :: Bool
   , tags :: [Tag]
   } deriving (Show, Eq)

data Slide = HSlide Image | VSlide (Image, Image) deriving (Eq)

main :: IO ()
main = interact $ unlines . map show . arrange . toSlides . textFileToImages

textFileToImages :: String -> [Image]
textFileToImages s = setIndexes 0 $ map toImage $ tail . lines $ s
  where
    inputData = (tail . lines) s

toSlides :: [Image] -> [Slide]
toSlides images = (map HSlide his) ++ (map VSlide vis)
  where 
    his = filter horizontal images
    vis = pairUp $ filter (not . horizontal) images

arrange :: [Slide] -> [Slide]
arrange (x:xs)
    | length xs == 0 = [x]
    | otherwise = x : arrange rest
  where
    rest = match : (without match xs)
    match = bestMatch x xs

-----
toImage :: String -> Image
toImage s = Image index horizontal tags
  where
    index = 0
    ws = words s
    horizontal = head ws == "H"
    tags = tail (tail ws)

setIndexes :: Int -> [Image] -> [Image]
setIndexes _ [] = []
setIndexes n ((Image i h t):xs) = (Image n h t) : setIndexes (n + 1) xs

---

without :: (Eq a) => a -> [a] -> [a]
without n xs = filter (\i -> i /= n) xs

bestMatch :: Slide -> [Slide] -> Slide
bestMatch x xs = maximumBy (\a b -> compare (getScore x a) (getScore x b)) xs

getScore :: Slide -> Slide -> Int
getScore a b = min3 (length atags) (length btags) (length bothtags)
  where
    atags = getTags a
    btags = getTags b
    bothtags = intersect atags btags
    min3 x y z = min (min x y) z

getTags :: Slide -> [Tag]
getTags (HSlide i) = tags i
getTags (VSlide (a, b)) = union (tags a) (tags b)

instance Show Slide where
    show (HSlide (Image i _ _)) = show i
    show (VSlide ((Image ai _ _), (Image bi _ _))) = show ai ++ " " ++ show bi

pairUp :: [x] -> [(x, x)]
pairUp [] = []
pairUp (x:y:xs) = (x, y) : pairUp(xs)
pairUp (x:[]) = error "odd input array length"
