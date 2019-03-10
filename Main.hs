import Data.List

type Tag = String

data Image = Image
   { index :: Int
   , horizontal :: Bool
   , tags :: [Tag]
   } deriving (Show, Eq)

data Slide = HSlide Image | VSlide (Image, Image) deriving (Eq)

main :: IO ()
main = interact $ slidesToText . arrange . textToSlides
  where
    textToSlides = toSlides . toImages . tail . lines
    slidesToText = unlines . map show

toImages :: [String] -> [Image]
toImages = zipWith toImage [0..]

toSlides :: [Image] -> [Slide]
toSlides images = (map HSlide his) ++ (map VSlide vis)
  where 
    his = filter horizontal images
    vis = pairUp $ filter (not . horizontal) images

arrange :: [Slide] -> [Slide]
arrange [x] = [x]
arrange (x:xs) = x : arrange rest
  where
    rest = match : (without match xs)
    match = bestMatch x xs

toImage :: Int -> String -> Image
toImage index s = Image index (orientation == "H") tags
  where (orientation:_:tags) = words s

without :: (Eq a) => a -> [a] -> [a]
without n xs = filter (\i -> i /= n) xs

bestMatch :: Slide -> [Slide] -> Slide
bestMatch candidate list = maximumBy (compareSlides candidate) list

compareSlides :: Slide -> Slide -> Slide -> Ordering
compareSlides s a b = compare (getScore s a) (getScore s b)

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

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x:y:xs) = (x, y) : pairUp(xs)
pairUp (x:[]) = error "odd input array length"
