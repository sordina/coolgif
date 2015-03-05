
-- Constants

xrange, yrange :: [Float]
xrange = [0..100]
yrange = [0..100]

width, height :: Int
width  = length xrange
height = length yrange

maxVal :: Float
maxVal = 6

totalFrames :: Int
totalFrames = 50

-- Program

main :: IO ()
main = sequence_ $ zipWith writeImage [(1 :: Int) ..] images

writeImage :: Show a => a -> String -> IO ()
writeImage index image = writeFile ("frames/coolgif-" ++ pad 5 (show index) ++ ".pgm") image

pad :: Int -> [Char] -> [Char]
pad n s = if length s > n then s
                          else reverse $ take n $ reverse s ++ replicate n '0'

images :: [String]
images = fmap renderFrame frames

renderFrame :: [[Float]] -> String
renderFrame rs = unlines [ "P2"
                         , show width ++ " " ++ show height
                         , show maxVal
                         , unlines (map renderRow rs)
                         ]

renderRow :: [Float] -> String
renderRow r = unwords $ map renderPixel r

type F = Float -> Int

renderPixel :: Float -> String
renderPixel = show . f1

f1 :: Float -> Int
f1 t = round r
  where r :: Float
        r = maxVal * (t + 1) / 2

frames :: [[[Float]]]
frames = map intensities times

times :: [Float]
times = splitup totalFrames 0 (4 * pi)

splitup :: Int -> Float -> Float -> [Float]
splitup n a b = take n (map (* ((b - a) / fromIntegral n)) [0..])

intensities :: Float -> [[Float]]
intensities t =
  for yrange $ \y ->
    for xrange $ \x ->
      pixel t x y

pixel :: Float -> Float -> Float -> Float
pixel t x y = avg [ sin $ (x + y) / 50 + t
                  , sin $ (x - y) / 50 - (t/2) ]

avg :: [ Float ] -> Float
avg a = sum a / fromIntegral (length a)

-- utils

for :: [a] -> (a -> b) -> [b]
for = flip map
