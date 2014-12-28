import Codec.Picture
import Codec.Picture.Types
import Data.Word
import qualified Data.ByteString as BS
import Text.Printf

-- Convert pictures of 2 space dimmensions and 1 grayscale color dimmension
-- to a gnuplot friendly format.

fold = (\a x y w -> (printf "%d %d %d" x y w) : a)

datify :: DynamicImage -> String
datify (ImageY8 i) = foldr1 (\a b -> a ++ "\n" ++ b) . reverse . pixelFold fold [] $ i
datify _           = error "Non-grayscale images not supported"

main = putStrLn . either error datify . decodeImage =<< BS.getContents
