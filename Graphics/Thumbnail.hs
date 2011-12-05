module Graphics.Thumbnail
       ( ImageFormat(..)
       , Thumbnail(..)
       , mkThumbnail) where

import Graphics.GD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

data ImageFormat = Gif | Jpeg | Png

data Thumbnail = Thumbnail { fmt :: ImageFormat
                           , img :: Image
                           , sz  :: Size
                           , lbs :: L.ByteString
                           }

mkThumbnail :: L.ByteString -> IO Thumbnail
mkThumbnail = thumbnail . L.unpack
  where
    thumbnail ws@(0xff:0xd8:_) = thumbnailJpeg ws
    thumbnail ws@(0x89:0x50:_) = thumbnailPng ws
    thumbnail ws@(0x47:0x49:0x46:_) = thumbnailGif ws
    thumbnail _ = error "unsupported image format"
    
    thumbnailJpeg ws = do
      src <- loadJpegByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- saveJpegByteString (-1) thm
      return Thumbnail { fmt=Jpeg
                       , img=thm
                       , sz=size'
                       , lbs=strictToLazy bs
                       }
    
    thumbnailPng ws = do
      src <- loadPngByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- savePngByteString thm
      return Thumbnail { fmt=Png
                       , img=thm
                       , sz=size'
                       , lbs=strictToLazy bs
                       }
      
    thumbnailGif ws = do
      src <- loadGifByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- saveGifByteString thm
      return Thumbnail { fmt=Gif
                       , img=thm
                       , sz=size'
                       , lbs=strictToLazy bs
                       }
        
    strictToLazy = L.pack . BS.unpack
    
newSize :: Size -> Size
newSize (w, h) | w >= h && wMax*h`div`w > wMin = (wMax, wMax*h`div`w)
               | w >= h && h >= hMin           = (hMin*w`div`h, hMin)
               | w <  h && hMax*w`div`h > hMin = (hMax*w`div`h, hMax)
               | w <  h && w >= wMin           = (wMin, wMin*h`div`w)
               | otherwise = (w, h)

wMax, wMin, hMax, hMin :: Int
(wMax, wMin) = (60, 20)
(hMax, hMin) = (60, 20)
