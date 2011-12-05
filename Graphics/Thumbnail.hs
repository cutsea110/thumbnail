module Graphics.Thumbnail
       ( ImageFormat(..)
       , Thumbnail(..)
       , mkThumbnail) where

import Graphics.GD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

data ImageFormat = Gif | Jpeg | Png

data Thumbnail = Thumbnail { fmt :: ImageFormat     -- ^ Image Format Type
                           , img :: Image           -- ^ Thumbnail Image
                           , sz  :: Size            -- ^ Thumbnail Size
                           , lbs :: L.ByteString    -- ^ Thumbnail Data
                           , orgImg :: Image        -- ^ Original Image
                           , orgSZ :: Size          -- ^ Original Size
                           }

mkThumbnail :: L.ByteString -> IO (Either String Thumbnail)
mkThumbnail = thumbnail . L.unpack
  where
    thumbnail ws@(0xff:0xd8:_) = thumbnailJpeg ws
    thumbnail ws@(0x89:0x50:_) = thumbnailPng ws
    thumbnail ws@(0x47:0x49:0x46:_) = thumbnailGif ws
    thumbnail _ = return $ Left "unsupported image format"
    
    thumbnailJpeg ws = do
      src <- loadJpegByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- saveJpegByteString (-1) thm
      return $ Right Thumbnail { fmt=Jpeg
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               }
    
    thumbnailPng ws = do
      src <- loadPngByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- savePngByteString thm
      return $ Right Thumbnail { fmt=Png
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               }
      
    thumbnailGif ws = do
      src <- loadGifByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize size
      thm <- uncurry resizeImage size' dest
      bs <- saveGifByteString thm
      return $ Right Thumbnail { fmt=Gif
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
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
