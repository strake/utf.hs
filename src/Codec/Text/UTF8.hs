{-# LANGUAGE BlockArguments #-}
module Codec.Text.UTF8 (Error, decode1) where

import Control.Arrow
import Data.Bits
import qualified Data.List as List
import Data.Word
import Util

decode1 :: [Word8] -> Maybe (Either Error Char, [Word8])
decode1 = List.uncons & fmap (uncurry (choose . fromEnum) >>> fmap toEnum *** id)
  where
    choose b bs
      | b < 0x80 = (Right $ toEnum b, drop 1 bs)
      | l < 2 || l > 6 = (Left InvalidSequence, drop 1 bs)
      | otherwise = go' l (fromEnum b .&. shiftR 0x7F l, bs)
      where
        l = countLeadingZeros (complement b)
        go (x, b:bs')
          | b .&. 0xC0 == 0x80 = (Right $ shiftL x 6 .|. fromEnum b .&. 0x3F, bs')
          | otherwise = (Left InvalidSequence, bs')
        go (_, bs) = (Left InvalidSequence, bs)
        go' 0 = Right *** id
        go' k = go & uncurry \ case
            Right x -> seq x $ go' (k-1) `curry` x
            z -> (,) z

data Error = InvalidSequence
  deriving (Eq, Show)
