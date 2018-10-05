module CPU where

import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as B
import Data.Bits
import CPU.Types
import CPU.Util
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
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
brk s = s { pc = readCPUmemory16 s 0xFFFE, b = 1}

adc :: CPU -> Address -> CPU
adc s@CPU{c = cv, acc = av, b = bv, z = zv} addr =
  let acc' = cv  + (readCPUmemory8 s addr) + av 
      shouldCarry = fromIntegral . fromEnum $ toInt av + toInt bv + toInt cv > 0xFF
      doesOverflow = fromIntegral . fromEnum $ ((av `xor` bv) .&. 0x80) == 0 &&
                                               ((av `xor` acc') .&. 0x80) /= 0
  in s{c   = shouldCarry
      ,v   = doesOverflow
      ,acc = acc'
      }

and :: CPU -> Address -> CPU
and s@CPU{acc = av} addr =
  s {acc = av .&. (readCPUmemory8 s addr)}

asl :: CPU -> AddressMode -> Address -> CPU
asl s@CPU{acc = av, v = vv} mode addr =
  let bit7 = bitGet 8 av 
      acc' = av `shiftL` 1
      bit7' = bitGet 8 acc'  
  in  case mode of
    Accumulator -> s {acc = acc'
                      ,c = bit7
                      ,v = (if bit7' == 1 then 1 else vv)}
    _           -> s {memory = writeByte (memory s) addr (readCPUmemory8 s addr)
                      ,c = bit7
                      ,v = (if bit7' == 1 then 1 else vv)}

bcc :: CPU -> Address -> CPU
bcc s@CPU{c = cv} addr = branch s (not $ toBool cv) addr

bcs :: CPU -> Address -> CPU
bcs s@CPU{c = cv} addr = branch s (toBool cv) addr

beq :: CPU -> Address -> CPU
beq s@CPU{z = zv} addr = branch s (toBool zv) addr

bit :: CPU -> Address -> CPU
bit s@CPU{acc = av} addr = if 0 == (av .&. readCPUmemory8 s addr)
                           then s {z = 1, n = bit7, v = bit6}
                           else s {z = 0, n = bit7, v = bit6}
                           where bit7 = bitGet 8 av
                                 bit6 = bitGet 7 av

bmi :: CPU -> Address -> CPU
bmi s@CPU{n = nv} addr = branch s (toBool nv) addr

bne :: CPU -> Address -> CPU
bne s@CPU{z = zv} addr = branch s (not $ toBool zv) addr

bpl :: CPU -> Address -> CPU
bpl s@CPU{n = nv} addr = branch s (not $ toBool nv) addr

bvc :: CPU -> Address -> CPU
bvc s@CPU{v = vv} addr = branch s (not $ toBool vv) addr

bvs :: CPU -> Address -> CPU
bvs s@CPU{v = vv} addr = branch s (toBool vv) addr

clc :: CPU -> CPU
clc s = s {c = 0}

cld :: CPU -> CPU
cld s = s {d = 0}

cli :: CPU -> CPU
cli s = s {i = 0}

clv :: CPU -> CPU
clv s = s {v = 0}

cmp :: CPU -> Address -> CPU
cmp s@CPU{acc = av} addr =
  compare' s av $ readCPUmemory8 s addr

cpx :: CPU -> Address -> CPU
cpx s@CPU{x = xv} addr =
  compare' s xv $ readCPUmemory8 s addr

cpy :: CPU -> Address -> CPU
cpy s@CPU{y = yv} addr =
  compare' s yv $ readCPUmemory8 s addr

dec :: CPU -> Address -> CPU
dec s addr = let sv = decrement s (readCPUmemory8 s addr)
                 s' = fst sv
                 val= snd sv in
               s' {memory = writeByte (memory s) addr val}
                 

dex :: CPU -> CPU
dex s = let sv = decrement s (x s)
            s' = fst sv
            val = snd sv in
          s' {x = val}

dey :: CPU -> CPU
dey s = let sv = decrement s (y s)
            s' = fst sv
            val = snd sv in
          s' {y = val}

eor :: CPU -> Address -> CPU
eor s@CPU{acc = a, z = zv, n = nv} addr =
  s {acc = val
    ,z = if val == 0 then 1 else zv
    ,n = if bitGet 8 val == 1 then 1 else nv}
  where val = xor a (readCPUmemory8 s addr)

inc :: CPU -> Address -> CPU
inc s addr = let sv = augment s (readCPUmemory8 s addr)
                 s' = fst sv
                 val= snd sv in
               s' {memory = (writeByte (memory s) addr val)} 

inx :: CPU -> CPU
inx s = let sv = augment s (x s)
            s' = fst sv
            val= snd sv in
          s' {x = val}

iny :: CPU -> CPU
iny s = let sv = augment s (y s)
            s' = fst sv
            val= snd sv in
          s' {y = val}

jsr :: CPU -> Address -> CPU
jsr s@CPU{pc = pcv} addr =
  pushAddr s pcv

jmp :: CPU -> Address -> CPU
jmp s addr = s {pc = addr}

lda :: CPU -> Address -> CPU
lda s addr =
  let sv = arithmetic (\_ -> (readCPUmemory8 s addr)) s (acc s)
      s' = fst sv
      val= snd sv
  in s' {acc = val}

ldx :: CPU -> Address -> CPU
ldx s addr =
  let sv = arithmetic (\_ -> (readCPUmemory8 s addr)) s (x s)
      s' = fst sv
      val= snd sv
  in s' {x = val}

ldy :: CPU -> Address -> CPU
ldy s addr =
  let sv = arithmetic (\_ -> (readCPUmemory8 s addr)) s (y s)
      s' = fst sv
      val= snd sv
  in s' {y = val}

lsr :: CPU -> AddressMode -> Address -> CPU
lsr s@CPU{acc = av, v = vv} mode addr =
  let bit7 = bitGet 8 av 
      acc' = av `shiftR` 1
      bit7' = bitGet 8 acc'  
  in  case mode of
    Accumulator -> s {acc = acc'
                      ,c = bit7
                      ,v = (if bit7' == 1 then 1 else vv)}
    _           -> s {memory = writeByte (memory s) addr (readCPUmemory8 s addr)
                      ,c = bit7
                      ,v = (if bit7' == 1 then 1 else vv)}

nop :: CPU -> CPU
nop s = s

ora :: CPU -> Address -> CPU
ora s@CPU{acc = a} addr = let sv = arithmetic (.&.a) s (readCPUmemory8 s addr)
                              s' = fst sv
                              val= snd sv in
                            s' {acc = val}

pha :: CPU -> CPU
pha s@CPU{acc = a} = push s a 

php :: CPU -> CPU
php s = push s (flagsToByte s)

pla :: CPU -> CPU
pla s = pop s 


--auxilliary functions

push :: CPU -> Byte -> State CPU
push s@CPU{memory = mem, sp = spv} val =
  s {memory = writeByte mem ((toWord16 spv) + 0x0100) val
    ,sp = spv + 1}

pop :: CPU -> CPU
pop s@CPU{sp = spv, memory = mem} =
  s {acc = readCPUmemory8 s ((toWord16 spv) - 1 + 0x0100),
     sp = spv - 1, memory = (writeByte mem (toWord16 spv -1 + 0x0100) 0)}

pushAddr :: CPU -> Address -> CPU
pushAddr s@CPU{memory = mem, sp = spv} addr =
  s {memory = writeAddr mem ((toWord16 spv) + 0x0100) addr
    ,sp = spv +1}

branch :: CPU -> Bool -> Address -> CPU
branch s@CPU{cyclesC = cycles'} cond addr =
  let cycles'' = if differentPages (pc s) addr then 2 else 1
  in if cond
     then s {pc = addr, cyclesC = cycles'' + cycles'}
     else s 

compare' :: CPU -> Byte -> Byte -> CPU
compare' s v1 v2 =
  if v1 == v2
  then s {c = 1, z = 1}
  else
    if v1 > v2
    then s {c = 1}
    else s

arithmetic :: (Byte -> Byte) -> CPU -> Word8 -> (CPU,Byte) --memory arithmetic
arithmetic f s val = let val' = f val in 
                        if val' == 0
                        then (s {z = 1},val')
                        else
                          if bitGet 8 val == 1
                          then (s {n = 1},val')
                          else (s,val')

augment :: CPU -> Word8 -> (CPU,Word8)
augment s val = arithmetic (+1) s val

decrement :: CPU -> Word8 -> (CPU,Word8) 
decrement s val = arithmetic (1-) s val 
  
writeByte :: Memory -> Address -> Byte -> Memory
writeByte mem addr val = V.modify (\vec -> MV.write vec (fromIntegral addr) val) mem

writeAddr :: Memory -> Address -> Address -> Memory
writeAddr memory target val =
  runST $ do
    vector <- V.unsafeThaw $ memory
    MV.write vector (fromIntegral target) lo
    MV.write vector (fromIntegral target + 1) hi
    V.unsafeFreeze vector
  where pair = splitW16 val
        lo   = fst pair
        hi   = snd pair

readCPUmemory8 :: CPU -> Address -> State CPU Byte
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
