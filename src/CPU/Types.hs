module CPU.Types where

import Data.Word
import qualified Data.Vector as V

data CPU = CPU {
    memory   :: V.Vector Word8
  , acc      :: Byte --Accumulator
  , x        :: Byte --Index 
  , y        :: Byte --Index
  , sp       :: Byte --Stack Pointer
  , pc       :: Address
  --Flags
  , c        :: Bool
  , z        :: Bool
  , i        :: Bool
  , d        :: Bool
  , b        :: Bool
  , v        :: Bool
  , s        :: Bool
  }
  deriving Show

initCPU =
  CPU {
    memory = V.replicate (1024 * 16) 0x00
  , acc    = 0
  , x      = 0
  , y      = 0
  , sp     = 0
  , pc     = 0
  , c      = False
  , z      = False
  , i      = False
  , d      = False
  , b      = False
  , v      = False
  , s      = False
  }
  
type Byte = Word8
type Address = Word16
