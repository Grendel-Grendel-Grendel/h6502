module CPU where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Bits
import CPU.Types
import CPU.Util --toChar comes from here as stack didnt like ascii lib
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.List.Index (indexed)
import CPU.Opcode

--initial rom loading
getRom :: FilePath -> IO [Word8]
getRom fp = B.readFile fp >>= (return . B.unpack)

loadMemory :: [Word8] -> Int -> State CPU ()
loadMemory rom i = do
  s@CPU{memory = mem} <- get
  put s { memory = mem V.// (alignIndex rom)}
  return ()
  where
    alignIndex  = increaseFst . indexed
    increaseFst = fmap (\(x,y) -> (x+i,y))

--opcodes

brk :: State CPU IOevent
brk = do s <- get
         php
         pushAddr (pc s)  
         return Nothing 

adc :: Address -> State CPU IOevent
adc addr = do
  s@CPU{acc = a} <- get 
  val <- readCPUmemory8 addr
  let (a',s') = runState (arithmetic (+a) val) s
  put $ s' {acc = a', c = if bitGet 7 a /= bitGet 7 a' then 1 else c s'  }
  return Nothing  

and :: Address -> State CPU IOevent
and addr = do
  s@CPU{acc = av} <- get
  val <- readCPUmemory8 addr
  put $ s {acc = av .&. val}
  return Nothing

asl :: AddressMode -> Address -> State CPU IOevent
asl mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `shiftL` 1
      bit7' = bitGet 8 acc'  
  case mode of
    Accumulator -> do put $ s {acc = acc'
                              ,c = bit7
                              ,v = (if bit7' == 1 then 1 else vv)}
                      return Nothing
    _           -> do
      val <- readCPUmemory8 addr
      put $ s {c = bit7
              ,v = (if bit7' == 1 then 1 else vv)}
      writeByte addr val
        
bcc ::  Address -> State CPU IOevent
bcc addr = do
  s@CPU{c = cv} <- get
  branch (not $ toBool cv) addr

bcs :: Address -> State CPU IOevent
bcs addr = do
  s@CPU{c = cv} <- get
  branch (toBool cv) addr

beq :: Address -> State CPU IOevent
beq addr = do
  s@CPU{z = zv} <- get
  branch (toBool zv) addr

bit :: Address -> State CPU IOevent
bit addr = do
  s@CPU{acc = av} <- get
  val <- readCPUmemory8 addr
  let bit7 = bitGet 8 av
      bit6 = bitGet 7 av
  if 0 == (av .&. val)
    then do put $ s {z = 1, n = bit7, v = bit6}
            return Nothing
    else do put $ s {z = 0, n = bit7, v = bit6}
            return Nothing

bmi :: Address -> State CPU IOevent
bmi addr = do
  s@CPU{n = nv} <- get
  branch (toBool nv) addr

bne :: Address -> State CPU IOevent
bne addr = do
  s@CPU{z = zv} <- get 
  branch (not $ toBool zv) addr

bpl :: Address -> State CPU IOevent
bpl addr = do
  s@CPU{n = nv} <- get
  branch (not $ toBool nv) addr

bvc :: Address -> State CPU IOevent
bvc addr = do
  s@CPU{v = vv} <- get
  branch (not $ toBool vv) addr

bvs :: Address -> State CPU IOevent
bvs addr = do
  s@CPU{v = vv} <- get
  branch (toBool vv) addr

clc :: State CPU IOevent
clc = do s <- get
         put s {c = 0}
         return Nothing
           
cld :: State CPU IOevent
cld = do s <- get
         put s {d = 0}
         return Nothing
           
cli :: State CPU IOevent
cli = do s <- get
         put s {i = 0}
         return Nothing
           
clv :: State CPU IOevent
clv = do s <- get
         put s {v = 0}
         return Nothing
           
cmp :: Address -> State CPU IOevent
cmp addr = do s@CPU{acc = av} <- get
              val <- readCPUmemory8 addr
              compare' av val

cpx :: Address -> State CPU IOevent
cpx addr = do s@CPU{acc = av,x = xv} <- get
              val <- readCPUmemory8 addr
              compare' xv val

cpy :: Address -> State CPU IOevent
cpy addr = do
  s@CPU{y = yv} <- get
  val <- readCPUmemory8 addr
  compare' yv val 

dec :: Address -> State CPU IOevent
dec addr = do
  s <- get
  val <-  readCPUmemory8 addr
  let (a,s') = runState (decrement val) s 
  writeByte addr a

dex :: State CPU IOevent
dex = do
  s <- get
  let (a, s') = runState (decrement (x s)) s
  put $ s' {x = a}
  return Nothing

dey :: State CPU IOevent
dey = do s <- get
         let (a, s') = runState (decrement (y s)) s
         put $ s' {y = a}
         return Nothing

eor :: Address -> State CPU IOevent
eor addr = do
  s@CPU{acc = a, z = zv, n = nv} <- get
  y <- readCPUmemory8 addr
  let val = xor a y 
  put $ s {acc = val
          ,z = if val == 0 then 1 else zv
          ,n = if bitGet 8 val == 1 then 1 else nv}
  return Nothing

inc :: Address -> State CPU IOevent
inc addr = do val <- readCPUmemory8 addr
              augment val
              writeByte addr (val - 1) --augment does not write the byte, having val manually decreased results in cleaner code 
              

inx :: State CPU IOevent
inx = do s <- get
         let (a, s') = runState (augment (y s)) s
         put $ s' {y = a}
         return Nothing

iny :: State CPU IOevent
iny = do s <- get
         let (a, s') = runState (augment (y s)) s
         put $ s' {y = a}
         return Nothing

jsr :: Address -> State CPU IOevent
jsr addr = do CPU{pc = pcv} <- get 
              pushAddr (pcv - 1)
              jmp addr
              

jmp :: Address -> State CPU IOevent
jmp addr = do
  s@CPU{pc = pcv} <- get
  put $ s {pc = addr}
  return Nothing

lda :: Address -> State CPU IOevent
lda addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (acc s)) s
  put $ s' {acc = a}
  return Nothing

ldx :: Address -> State CPU IOevent
ldx addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (x s)) s
  put $ s' {x = a}
  return Nothing

ldy :: Address -> State CPU IOevent
ldy addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (y s)) s
  put $ s' {y = a}
  return Nothing
  
lsr :: AddressMode -> Address -> State CPU IOevent
lsr mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `shiftR` 1
      bit7' = bitGet 8 acc'
  val <- readCPUmemory8 addr    
  case mode of
    Accumulator -> do put s {acc = acc'
                            ,c = bit7
                            ,v = (if bit7' == 1 then 1 else vv)}
                      return Nothing
    _           -> do put s {c = bit7
                             ,v = (if bit7' == 1 then 1 else vv)}
                      writeByte addr val  

sre :: Address -> State CPU IOevent
sre addr = do
  s <- get
  val <- readCPUmemory8 addr
  let a  = val `shiftR` 1
      a' = xor a (acc s)
  lsr Immediate addr
  s' <- get
  let s'' = execState (arithmetic (xor a) (acc s)) s' 
  put s''{acc = a'}
  return Nothing

nop :: State CPU IOevent
nop = return Nothing

ora :: Address -> State CPU IOevent
ora addr = do
  s@CPU{acc = a} <- get
  val <- readCPUmemory8 addr
  let (a', s') = runState (arithmetic (a.|.) (x s)) s
  put $ s' {acc = a'}
  return Nothing

pha :: State CPU IOevent
pha = do CPU{acc = a} <- get
         push a
  

php :: State CPU IOevent
php = do s@CPU{acc = a} <- get
         push (flagsToByte s)

pla :: State CPU IOevent
pla = do
  s <- get 
  let s' = execState (pop) s
  put s'
  
  return Nothing

sec :: State CPU IOevent
sec = do
  s <- get
  put $ s {c = 1}
  return Nothing

slo :: Address -> State CPU IOevent
slo addr = do
  s <- get
  let (a,s') = runState (readCPUmemory8 addr) s
      r = (shiftL a 1) .|. (acc s')
  put s' {acc = r}
  asl Immediate addr
  return Nothing
  

plp :: State CPU IOevent
plp = byteToFlags

rol :: AddressMode -> Address -> State CPU IOevent
rol mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `rotateL` 1
      bit7' = bitGet 8 acc'  
  case mode of
    Accumulator -> do put $ s {acc = acc'
                              ,c = bit7
                              ,v = (if bit7' == 1 then 1 else vv)}
                      return Nothing
    _           -> do val <- readCPUmemory8 addr
                      let val7  = bitGet 8 val
                          val'  = val `rotateL` 1
                          val7' = bitGet 8 val'
                      put s{c = val7, v = (if val7' == 1 then 1 else vv)}
                      writeByte addr val'

ror :: AddressMode -> Address -> State CPU IOevent
ror mode addr = do
  s@CPU{acc = av, v = vv} <- get  
  case mode of
    Accumulator -> do
      let bit7 = bitGet 8 av 
          av' = av `rotateR` 1
          bit7' = bitGet 8 av'
      put $ s {acc = av'
              ,c = bit7
              ,v = (if bit7' == 1 then 1 else vv)}
      return Nothing
    _           -> do
      val <- readCPUmemory8 addr
      let bit7 = bitGet 8 val 
          val' = val `rotateR` 1
          bit7' = bitGet 8 val'
      put s {c = bit7
            ,v = (if bit7' == 1 then 1 else vv)}
      writeByte addr val'

rra :: Address -> State CPU IOevent
rra addr = do
  ror (Immediate) addr
  adc addr

rti :: State CPU IOevent
rti = do  
  rts
  plp

rts :: State CPU IOevent
rts = do
  s@CPU{sp = spv, memory = mem} <- get
  let val = evalState (readCPUmemory16 (toWord16 (spv - 2))) s
  put s {memory = writeAddr mem (toWord16 spv) 0}
  jmp val

sbc :: Address -> State CPU IOevent
sbc addr = do
  s@CPU{acc = a} <- get 
  val <- readCPUmemory8 addr
  let (a',s') = runState (arithmetic ((-)a) val) s
  put $ s' {acc = a', c = if bitGet 7 a /= bitGet 7 a' then 0 else c s'  }
  return Nothing

sed :: State CPU IOevent
sed = do
  s <- get
  put $ s {d = 1}
  return Nothing

sei :: State CPU IOevent
sei = do
  s <- get
  put $ s {i = 1}
  return Nothing

sta :: Address -> State CPU IOevent
sta addr = do
  s@CPU{memory = mem,acc = a} <- get
  writeByte addr a

stx :: Address -> State CPU IOevent
stx addr = do
  s@CPU{memory = mem, x = xv} <- get
  writeByte addr xv
  
sty :: Address -> State CPU IOevent
sty addr = do
  s@CPU{memory = mem, y = yv} <- get
  writeByte addr yv

tax :: State CPU IOevent
tax = do
  s@CPU{acc = a, x = xv} <- get
  let s' = execState (arithmetic (\_ -> a) xv) s
  put s' {x = a}
  return Nothing

tay :: State CPU IOevent
tay = do
  s@CPU{acc = a,y = yv} <- get
  let s' = execState (arithmetic (\_ -> a) yv) s
  put $ s' {y = a}
  return Nothing

tsx :: State CPU IOevent
tsx = do
  s@CPU{sp = spv, x = xv} <- get
  let s' = execState (arithmetic (\_ -> spv) xv) s
  put s'{x = spv}
  return Nothing

txa :: State CPU IOevent
txa = do
  s@CPU{acc = a, x = xv} <- get
  let s' = execState (arithmetic (\_ -> xv) a) s
  put $ s {acc = xv}
  return Nothing

txs :: State CPU IOevent
txs = do
  s@CPU{x = xv,sp = spv} <- get
  let s' = execState (arithmetic (\_ -> xv) spv ) s
  put $ s {sp = xv}
  return Nothing

tya :: State CPU IOevent
tya = do
  s@CPU{acc = a, y = yv} <- get
  let s' = execState (arithmetic (\_ -> yv) a) s
  put $ s {acc = yv}
  return Nothing


--auxilliary functions

byteToFlags :: State CPU IOevent 
byteToFlags = do
  s@CPU{sp = spv} <- get
  bt <- readCPUmemory8 (toWord16 spv)
  put s {c = bitGet 0 bt
        ,z = bitGet 1 bt
        ,i = bitGet 2 bt
        ,d = bitGet 3 bt
        ,b = bitGet 4 bt
        ,v = bitGet 6 bt 
        ,n = bitGet 7 bt
        ,memory = writeAddr (memory s) (toWord16 spv) 0} 
  return Nothing                                 

push :: Byte -> State CPU IOevent
push val = do
  s@CPU{memory = mem, sp = spv} <- get
  put $ s {sp = spv + 1}
  writeByte ((toWord16 spv) + 0x0100) val --rewrite other writes to memory that use let statements to be in this style

pop :: State CPU IOevent
pop = do
  a@CPU{sp = spv,memory = mem} <- get
  val <- readCPUmemory8 ((toWord16 spv) - 1 + 0x0100)
  put $ a {acc = val,
           sp = spv - 1}
  writeByte (toWord16 spv -1 + 0x0100) 0

pushAddr :: Address -> State CPU IOevent
pushAddr addr = do s@CPU{memory = mem, sp = spv} <- get
                   put s {memory = writeAddr mem ((toWord16 spv) + 0x0100) addr
                         ,sp = spv +2}
                   return Nothing

branch :: Bool -> Address -> State CPU IOevent
branch cond addr = do
  s@CPU{cyclesC = cycles',pc = pcv} <- get
  val <- (readCPUmemory8 addr)
  let cycles'' = if differentPages (pc s) addr then 2 else 1
  if cond
    then do put s {pc = (fromIntegral val) + pcv , cyclesC = cycles'' + cycles'}
            return Nothing
    else do
    put s{cyclesC = cycles' + 2,pc = pcv + 2}
    return Nothing

compare' :: Byte -> Byte -> State CPU IOevent
compare' v1 v2 = do
  s <- get
  if v1 > v2
    then do put s {c = 1}
            return Nothing
    else
    if v1 == v2
    then do put s {c = 1, z = 1}
            return Nothing
    else do
      put s {n = 1}
      return Nothing

--takes a function and a byte and runs tests to change flags. Returns result of f
--especially useful for things which alter the acc
arithmetic :: (Byte -> Byte) -> Byte -> State CPU Byte
arithmetic f val = do
  let val' = f val
  s <- get
  if val' == 0
    then do put $ s {z = 1}
            return val'
    else if bitGet 7 val' == 1
         then do put $ s {n = 1}
                 return val'
         else return val'

--note that the arithmetical aux functions do not write the values they operate upon, the operation itself must do that.
augment :: Word8 -> State CPU Word8
augment val = arithmetic (+1) val

decrement :: Word8 -> State CPU Byte
decrement val = do
  arithmetic (val-) 1 --subtraction isn't associative dingus
  
writeByte :: Address -> Byte -> State CPU IOevent
writeByte addr val =
  do s@CPU{memory=mem} <- get
     if addr == ioOut
       then
       do let mem' = V.modify (\vec -> MV.write vec (fromIntegral addr) val) mem
          put s {memory=mem'}
          return $ Just (putChar (toChar val))       
       else
       do let mem' = V.modify (\vec -> MV.write vec (fromIntegral addr) val) mem
          put s {memory=mem'}
          return Nothing
writeAddr :: Memory -> Address -> Address -> Memory
writeAddr memory target val =
  runST $ do
    vector <- V.unsafeThaw memory
    MV.write vector (fromIntegral target) lo
    MV.write vector (fromIntegral target + 1) hi
    V.unsafeFreeze vector
  where pair = splitW16 val
        lo   = fst pair
        hi   = snd pair

readCPUmemory8 :: Address -> State CPU Byte
readCPUmemory8 i = do
  CPU{memory = mem} <- get
  return $ mem V.! (fromIntegral i)

readCPUmemory16 :: Address -> State CPU Address
readCPUmemory16 i = do
  lo <- readCPUmemory8 i 
  hi <- readCPUmemory8 (i+1)
  return (makeW16 lo hi)

read16Bug :: Address -> State CPU Address
read16Bug addr = do
  lo <- readCPUmemory8 addr
  hi <- readCPUmemory8 $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
  return $  makeW16 lo hi

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
    Indirect    -> (False, evalState (read16Bug immAddr) s)
    IndexedIndirect -> (False, evalState (read16Bug (immAddr + fromIntegral xr))s)
    IndirectIndexed -> let addr = evalState (read16Bug (immAddr + toWord16 yr)) s
                           different = differentPages immAddr addr 
                       in (different,addr)

  --immediate addr and byte
  where immAddr = evalState (readCPUmemory16 (pcv+1)) s
        immByte = evalState (readCPUmemory8  (pcv+1)) s

step :: State CPU (Opcode,Address,Byte,IOevent)
step = do
  s@CPU{memory = mem, pc = pcv} <- get
  byte <- readCPUmemory8 pcv
  let opcode@Opcode {CPU.Opcode.cycle = cycles,len = length, pageCrossCycles = extraCycles,mnem = mnemonic} = decodeOpcode byte
      op = getInstruction opcode
      (diff,address) = addressForMode s (mode opcode)
  byte' <- readCPUmemory8 (pcv + 1)
  let increasePc =
        if diff --done if not a jump or branch
        then do let s' = execState (op address) s
                put s' {cyclesC = (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles)
                                  + (fromIntegral extraCycles),
                         pc = (fromIntegral $ pc s') + (fromIntegral length)}
                return (opcode,address,byte',Nothing)
        else do let s' = execState (op address) s
                put s' {cyclesC = fromIntegral (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles),
                         pc = (fromIntegral $ pc s')
                              + (fromIntegral length)}
                return (opcode,address,byte',Nothing)
      samePc =
        if diff --done if not a jump or branch
        then do let s' = execState (op address) s
                put s' {cyclesC = (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles)
                                  + (fromIntegral extraCycles),
                         pc = (fromIntegral $ pc s')
                              + (fromIntegral length)}
                return (opcode,address,byte',Nothing)
        else do let s' = execState (op address) s
                put s' {cyclesC = (cyclesC s')
                                  + (fromIntegral cycles)}
                return (opcode,address,byte',Nothing)
      samePcIO :: State CPU (Opcode,Address,Byte,IOevent) 
      samePcIO =
        if diff --done if not a jump or branch
        then do let (ioA,s') = runState (op address) s
                put s' {cyclesC = (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles)
                                  + (fromIntegral extraCycles),
                         pc = (fromIntegral $ pc s') + (fromIntegral length)}
                return (opcode,address,byte',ioA)
        else do let (ioA,s') = runState (op address) s
                put s' {cyclesC = fromIntegral (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles),
                         pc = (fromIntegral $ pc s')
                              + (fromIntegral length)}
                return (opcode,address,byte',ioA)
  case mnemonic of
    BNE -> samePc 
    BEQ -> samePc
    BMI -> samePc
    BPL -> samePc
    BRK -> samePc
    BVC -> samePc
    BVS -> samePc
    JMP -> samePc
    JSR -> samePc
    RTS -> samePc
    _   -> increasePc

getInstruction :: Opcode -> (Address -> State CPU IOevent)
getInstruction (Opcode _ mnemonic mode _ _ _) = case mnemonic of
  ADC -> adc
  AND -> CPU.and
  ASL -> asl mode
  BCC -> bcc
  BCS -> bcs
  BEQ -> beq
  BIT -> CPU.bit
  BMI -> bmi
  BNE -> bne
  BPL -> bpl
  BRK -> const brk
  BVC -> bvc
  BVS -> bvs
  CLC -> const clc
  CLD -> const cld
  CLI -> const cli
  CLV -> const clv
  CMP -> cmp
  CPX -> cpx
  CPY -> cpy
  DEC -> dec
  DEX -> const dex
  DEY -> const dey
  EOR -> eor
  INC -> inc
  INX -> const inx
  INY -> const iny
  JMP -> jmp
  JSR -> jsr
  LDA -> lda
  LDX -> ldx
  LDY -> ldy
  LSR -> lsr mode
  NOP -> const nop
  PHA -> const pha
  PHP -> const php
  PLA -> const pla
  PLP -> const plp
  ORA -> ora
  RTI -> const rti
  RTS -> const rts
  ROR -> ror mode
  ROL -> rol mode
  RRA -> rra
  SBC -> sbc
  SEC -> const sec
  SED -> const sed
  SEI -> const sei
  SLO -> slo
  STA -> sta
  STX -> stx
  STY -> sty
  TAX -> const tax
  TAY -> const tay
  TSX -> const tsx
  TXA -> const txa
  TXS -> const txs
  TYA -> const tya
  _   -> const nop
  
--Constants
ioRange :: (Address,Address)
ioRange = (0x800,0x0900)

ioOut = 0x1000
  
  
-- test  
  
  
