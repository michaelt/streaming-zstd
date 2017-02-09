module Streaming.Zstd 
  (compress
  , decompress
  , Z.maxCLevel
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Streaming.Internal(ByteString (..))
import qualified Codec.Compression.Zstd.Streaming as Z
import Codec.Compression.Zstd.Streaming ( Result(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)

-- | Compress a payload.  The input will be consumed lazily, and the
-- compressed result generated lazily.
--
-- /Note:/ if any error occurs, compression will fail part-way through
-- with a call to 'error'.
compress :: MonadIO m 
         =>  Int 
         -- ^ Compression level. Must be >= 1 and <= 'maxCLevel'.
         -> ByteString m r 
         -- ^ Payload to compress. It will be consumed chunkwise
         -> ByteString m r
compress n bs = liftIO (Z.compress n) >>= stream bs 

-- | Decompress a payload.  The input will be consumed lazily, and the
-- decompressed result generated lazily.
--
-- /Note:/ if any error occurs, decompression will fail part-way
-- through with a call to 'error'.
decompress :: MonadIO m => ByteString m r -> ByteString m r
decompress bs = liftIO (Z.decompress) >>= stream bs

stream :: MonadIO m => ByteString m r -> Result -> ByteString m r
stream (Go m) res = lift m >>= flip stream res
stream bs (Error who what) =  error (who ++ ": " ++ what)
stream bs (Produce bytes res') = Chunk bytes (liftIO res' >>= stream bs)
stream (Chunk c cs) (Consume f) = liftIO (f c) >>= stream cs
stream (Empty r) (Consume f)    = liftIO (f mempty) >>= stream (Empty r)
stream (Empty r) (Done o) =  Chunk o (Empty r)
stream input state = error $ "unpossible! bytes of input left in stream state "
                         ++ show state
