module CPU.Types where

import Data.Bits (shiftL,(.|.))
import Data.Word
import qualified Data.Vector.Unboxed as V

data CPU = CPU {
    memory    :: Memory
  , acc       :: Byte --Accumulator
  , x         :: Byte --Index 
  , y         :: Byte --Index
  , sp        :: Byte --Stack Pointer
  , pc        :: Address
  --Flags
  , c         :: Word8
  , z         :: Word8
  , i         :: Word8
  , d         :: Word8
  , b         :: Word8
  , v         :: Word8
  , n         :: Word8
  , cyclesC   :: Integer
  }
  deriving Show

initCPU =
  CPU {
    memory = V.replicate (10 * 1) 0x00
  , acc     = 0
  , x       = 0
  , y       = 0
  , sp      = 0
  , pc      = 0
  , c       = 0
  , z       = 0
  , i       = 0
  , d       = 0
  , b       = 0
  , v       = 0
  , n       = 0
  , cyclesC = 0
  }
  
type Byte = Word8
type Address = Word16
type Memory = V.Vector Word8

toBool :: (Num a, Eq a) => a -> Bool
toBool 1 = True
toBool _ = False

flagsToByte :: CPU -> Byte
flagsToByte s@CPU {c = c0
                  ,z = z1
                  ,i = i2
                  ,d = d3
                  ,b = b4
                  ,v = v6
                  ,n = n7} = let bitList = [c0
                                           ,shiftL z1 1
                                           ,shiftL i2 2
                                           ,shiftL d3 3
                                           ,shiftL b4 3
                                           ,1
                                           ,shiftL v6 6
                                           ,shiftL n7 7] in
                               foldr (.|.) 0 bitList
          
