module Compressor where

import Types

imageCompressor :: InputArgs -> IO ()
imageCompressor (InputArgs n l f) = print n