import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Loops
import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Word
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import Text.Printf

steps = 10000 :: Int
t     = 0.2

pdeStencil = makeStencil2 3 3
    (\ix -> case ix of
                Z :. -1 :.  0  -> Just 1
                Z :.  0 :. -1  -> Just 1
                Z :.  0 :.  1  -> Just 1
                Z :.  1 :.  0  -> Just 1
                Z :.  0 :.  0  -> Just (-4)
                _              -> Nothing)

diffuse :: (Int, (Array U DIM2 Double)) -> IO (Int, (Array U DIM2 Double))
diffuse (n, a) = do
    maped  <- computeP (mapStencil2 (BoundFixed 0) pdeStencil a) :: IO (Array U DIM2 Double)
    slowed <- computeP (Repa.map (*t) maped) :: IO (Array U DIM2 Double)

    (,) (n + 1) `fmap` (computeP $ a +^ slowed)

imageShape :: Image a -> DIM2
imageShape i = (Z :. imageHeight i) :. imageWidth i

numerify :: Image Word8 -> Array U DIM2 Double
numerify = flip (fromListUnboxed . imageShape) =<< reverse . pixelFold (\a _ _ p -> fromIntegral p : a) []

monochromify :: DynamicImage -> Image Word8
monochromify (ImageRGBA8 i) = pixelMap (\(PixelRGBA8 r _ _ _) -> 1 - (r `div` 128)) i
monochromify (ImageRGB8 i)  = pixelMap (\(PixelRGB8  r _ _  ) -> 1 - (r `div` 128)) i

out :: Int -> Array U DIM2 Double -> IO ()
out nr arr = writePng name $ generateImage (\x y -> discretize $ arr ! ((Z :. y) :. x)) w h
    where
        [w, h]     = listOfShape . extent $ arr
        discretize = floor . (*255) :: Double -> Word8
        name       = printf ("frame-%0" ++ show zeros ++ "i.png") nr
        zeros      = 1 + floor (logBase 10 (fromIntegral steps))

main = either error (run . numerify . monochromify) . decodeImage =<< read
    where
        read   = BS.readFile "/dev/stdin"
        run    = iterateUntilM ((== steps) . fst) step . ((,) 0)
        step a = diffuse a >>= (\a' -> uncurry out a' >> return a')
