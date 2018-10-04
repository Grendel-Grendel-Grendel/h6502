module CPU where

import qualified Data.ByteString as B
import Data.Bits
import CPU.Types
import CPU.Util
import Data.Word
import qualified Data.Vector as V
import Data.List.Index (indexed)
import CPU.Opcode


--initial rom loading
getRom :: FilePath -> IO [Word8]
getRom fp = B.readFile fp >>= (return . B.unpack)

loadMemory :: CPU -> [Word8] -> Int -> CPU
loadMemory s@CPU{memory = mem} rom i =
  s { memory = mem V.// (alignIndex rom)}
  where
    alignIndex  = increaseFst . indexed
    increaseFst = fmap (\(x,y) -> (x+i,y))

--opcodes

brk :: CPU -> CPU
brk s = s { pc = readCPUmemory16 s 0xFFFE, b = True}

adc :: CPU -> Address -> CPU
adc 
--auxilliary functions

readCPUmemory8 :: CPU -> Address -> Byte
readCPUmemory8 s@CPU{memory = mem} i = mem V.! (fromIntegral i)

readCPUmemory16 :: CPU -> Address -> Address
readCPUmemory16 s i =
  let lo = readCPUmemory8 s i 
      hi = readCPUmemory8 s (i+1)
  in makeW16 lo hi

read16Bug :: CPU -> Address -> Address
read16Bug s addr =
  let lo = readCPUmemory8 s addr
      hi = readCPUmemory8 s $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
  in  makeW16 lo hi

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

addressForMode :: CPU -> AddressMode -> (Bool, Address)
addressForMode s@CPU {memory = mem, pc = pcv, x = xr, y = yr} mode =
  case mode of
    Implied -> (False, 0)
    Accumulator -> (False, 0)
    Immediate   -> (False, pcv+1)
    ZeroPage    -> (False, toWord16 immByte)
    ZeroPageX   -> (False, toWord16 immByte + (fromIntegral xr))
    ZeroPageY   -> (False, toWord16 immByte + (fromIntegral yr))
    Relative    -> let offset16 = immAddr
                       offset8  = firstNibble offset16
                       diff = if offset8 < 0x80 then 0 else 0x100
                   in (False, pcv + 2 + offset8 - diff)
    Absolute    -> (False, immAddr)
    AbsoluteX   -> let addr = immAddr + toWord16 xr
                       different = differentPages immAddr addr
                   in (different, addr)
    AbsoluteY   -> let addr = immAddr + toWord16 yr
                       different = differentPages immAddr addr
                   in (different, addr)
    Indirect    -> (False, read16Bug s immAddr)
    IndexedIndirect -> (False, read16Bug s $ immAddr + fromIntegral xr)
    IndirectIndexed -> let addr = read16Bug s (immAddr + toWord16 yr)
                           different = differentPages immAddr addr 
                       in (different,addr)

  --immediate addr and byte
  where immAddr = readCPUmemory16 s (pcv+1)
        immByte = readCPUmemory8  s (pcv+1)
