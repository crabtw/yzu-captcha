module YzuCaptcha where

import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.List
import Data.Word

import Graphics.UI.Gtk.Gdk.Pixbuf

data CharInfo = CharInfo {
        charPosX :: Int,
        charPosY :: Int,
        charWidth :: Int,
        charHeight :: Int,
        charColor :: Word32
    } deriving Show

gray, brown, white, black :: Word32
gray = 0xFFC0C0C0
brown = 0xFF13458B
white = 0xFFFFFFFF
black = 0xFF000000

filledColors :: [Word32]
filledColors = [
        0xFF0000FF, 0xFF00FF00, 0xFFFF0000,
        0xFF00FFFF, 0xFFFFFF00, 0xFFFF00FF
    ]

imgWidth, imgHeight :: Int
imgWidth = 16
imgHeight = 12

binarize :: Pixbuf -> IO Pixbuf
binarize img = do
    width <- pixbufGetWidth img
    height <- pixbufGetHeight img
    newImg <- pixbufNew ColorspaceRgb True 8 width height
    [pixels, pixels'] <- mapM pixbufGetPixels [img, newImg]

    let checkColor (p, p') = isCharColor p && isCharColor p'
        comb [] = []
        comb (x:xs) = [(x, y) | y <- xs] ++ comb xs

    forM_ [(x, y) | y <- [0..height-1], x <- [0..width-1]] $ \(x, y) -> do
        let fivePos = filter (checkBounds (width, height)) [
                    (x, y), (x-1, y), (x+1, y), (x, y-1), (x, y+1)
                ]
        (px:others) <- mapM (readArray pixels . posToIx width) fivePos
        let color = if isCharColor px
                        then black
                        else if shiftR px 24 /= 0xFF
                                then if or $ map checkColor $ comb others
                                        then black else white
                                else white
        writeArray pixels' (posToIx width (x, y)) color
    return newImg

isCharColor :: Word32 -> Bool
isCharColor px = px /= gray && px /= brown && px /= white &&
                 alpha == 0xFF
    where alpha = shiftR px 24

checkBounds :: (Int, Int) -> (Int, Int) -> Bool
checkBounds (width, height) (x, y) =
    if x >= 0 && x < width && y >= 0 && y < height
        then True else False

posToIx :: Int -> (Int, Int) -> Int
posToIx width (x, y) = x + y * width

flood :: Pixbuf -> (Int, Int) -> Word32 -> Word32 -> IO (Int, Int, Int, Int)
flood img (x, y) target filled = do
    width <- pixbufGetWidth img
    height <- pixbufGetHeight img
    pixels <- pixbufGetPixels img

    let initBounds = (x, x, y, y)
        ix = posToIx width (x, y)

    color <- readArray pixels ix
    if checkBounds (width, height) (x, y) && color == target
        then do
            writeArray pixels ix filled
            let findBounds (l, r, t, b) (x', y') = do
                    (l', r', t', b') <- flood img (x', y') target filled
                    let l'' = if l' < l then l' else l
                    let r'' = if r' > r then r' else r
                    let t'' = if t' < t then t' else t
                    let b'' = if b' > b then b' else b
                    return (l'', r'', t'', b'')
            foldM findBounds initBounds [
                    (x-1, y), (x+1, y), (x, y-1), (x, y+1),
                    (x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)
                ]
        else return initBounds

fillColor :: Pixbuf -> Word32 -> IO [CharInfo]
fillColor img target = do
    width <- pixbufGetWidth img
    pixels <- pixbufGetPixels img

    let moveX x y colors = do
            if x < width
                then do
                    px <- readArray pixels $ posToIx width (x, y)
                    if px == target
                        then do
                            let c:cs = colors
                            (l, r, t, b) <- flood img (x, y) target c
                            (next, css) <- moveX r y cs
                            let info = CharInfo (l+1) (t+1) (r-l-1) (b-t-1) c
                            return $ (info : next, css)
                        else moveX (x+1) y colors
                else return ([], colors)
        moveY _ [] = return []
        moveY colors (y:ys) = do
            (info, cs) <- moveX 0 y colors
            moveY cs ys >>= return . (info:)

    info <- liftM concat $ moveY filledColors [7, 12, 16]
    return $ filter (\i -> charHeight i >= (imgHeight `div` 2)) info

divPos :: Int -> CharInfo -> [CharInfo]
divPos num info = unfoldr findPos (info, num)
    where avg = charWidth info `div` num
          findPos (_, 0) = Nothing
          findPos (i, n) = Just (
                i { charWidth = w' }, 
                (
                    i { charPosX = charPosX i + w',
                        charWidth = w - w' },
                    n-1
                )
            )
            where w' = if avg * 2 > w then w else avg
                  w = charWidth i

locateChars :: [CharInfo] -> [CharInfo]
locateChars info
    | len < 4 = concat $ 
                    if len == 3 || len == 1 ||
                       (not $ null w2) && w1 - head w2 > 10
                        then flip map info $ \i ->
                            if w1 == charWidth i
                                then divPos num i else [i]
                        else map (divPos 2) info
    | otherwise = info
    where len = length info
          num = 4 - len + 1
          (w1:w2) = sortBy (\a b -> compare b a) ws
          ws = map charWidth info

split :: Pixbuf -> [CharInfo] -> IO [Pixbuf]
split img info = do
    pixels <- pixbufGetPixels img :: IO (PixbufData Int Word32)
    forM info $ \i -> do
        let x = charPosX i
            y = charPosY i
            w = charWidth i
            h = charHeight i
            c = charColor i
            w' = if w > imgWidth then imgWidth else w
            h' = if h > imgHeight then imgHeight else h

        new <- pixbufNew ColorspaceRgb True 8 w' h'
        pixbufCopyArea img x y w' h' new 0 0
        pixels' <- pixbufGetPixels new

        forM_ [posToIx w' (x, y) | y <- [0..h'-1], x <- [0..w'-1]] $
            \ix -> do
                px <- readArray pixels' ix
                writeArray pixels' ix $ if px == c then black else white

        new' <- pixbufNew ColorspaceRgb True 8 imgWidth imgHeight
        pixbufFill new' 0xFF 0xFF 0xFF 0xFF
        pixbufCopyArea new
            0 0 w' h' new'
            ((imgWidth - w') `div` 2) ((imgHeight - h') `div` 2)
        return new'

segment :: Pixbuf -> IO [Pixbuf]
segment img = do
    bin <- binarize img
    info <- liftM locateChars $ fillColor bin black
    chars <- split bin info
    return chars
