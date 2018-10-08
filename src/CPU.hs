module CPU where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
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

loadMemory :: [Word8] -> Int -> State CPU ()
loadMemory rom i = do
  s@CPU{memory = mem} <- get
  put s { memory = mem V.// (alignIndex rom)}
  return ()
  where
    alignIndex  = increaseFst . indexed
    increaseFst = fmap (\(x,y) -> (x+i,y))

--opcodes

brk :: State CPU ()
brk = do s <- get
         php
         pushAddr (pc s)  
         return () 

adc :: Address -> State CPU ()
adc addr = do
  s@CPU{acc = a} <- get 
  val <- readCPUmemory8 addr
  let (a',s') = runState (arithmetic (+a) val) s
  put $ s' {acc = a', c = if bitGet 7 a /= bitGet 7 a' then 1 else c s'  }
  return ()  

and :: Address -> State CPU ()
and addr = do
  s@CPU{acc = av} <- get
  val <- readCPUmemory8 addr
  put $ s {acc = av .&. val}
  return ()

asl :: AddressMode -> Address -> State CPU ()
asl mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `shiftL` 1
      bit7' = bitGet 8 acc'  
  case mode of
    Accumulator -> do put $ s {acc = acc'
                              ,c = bit7
                              ,v = (if bit7' == 1 then 1 else vv)}
                      return ()
    _           -> do
      val <- readCPUmemory8 addr
      put $ s {memory = writeByte (memory s) addr val
              ,c = bit7
              ,v = (if bit7' == 1 then 1 else vv)}
      return ()
        
bcc ::  Address -> State CPU ()
bcc addr = do
  s@CPU{c = cv} <- get
  branch (not $ toBool cv) addr

bcs :: Address -> State CPU ()
bcs addr = do
  s@CPU{c = cv} <- get
  branch (toBool cv) addr

beq :: Address -> State CPU ()
beq addr = do
  s@CPU{z = zv} <- get
  branch (toBool zv) addr

bit :: Address -> State CPU ()
bit addr = do
  s@CPU{acc = av} <- get
  val <- readCPUmemory8 addr
  let bit7 = bitGet 8 av
      bit6 = bitGet 7 av
  if 0 == (av .&. val)
    then do put $ s {z = 1, n = bit7, v = bit6}
            return ()
    else do put $ s {z = 0, n = bit7, v = bit6}
            return ()

bmi :: Address -> State CPU ()
bmi addr = do
  s@CPU{n = nv} <- get
  branch (toBool nv) addr

bne :: Address -> State CPU ()
bne addr = do
  s@CPU{z = zv} <- get 
  branch (not $ toBool zv) addr

bpl :: Address -> State CPU ()
bpl addr = do
  s@CPU{n = nv} <- get
  branch (not $ toBool nv) addr

bvc :: Address -> State CPU ()
bvc addr = do
  s@CPU{v = vv} <- get
  branch (not $ toBool vv) addr

bvs :: Address -> State CPU ()
bvs addr = do
  s@CPU{v = vv} <- get
  branch (toBool vv) addr

clc :: State CPU ()
clc = do s <- get
         put s {c = 0}
         return ()
           
cld :: State CPU ()
cld = do s <- get
         put s {d = 0}
         return ()
           
cli :: State CPU ()
cli = do s <- get
         put s {i = 0}
         return ()
           
clv :: State CPU ()
clv = do s <- get
         put s {v = 0}
         return ()
           
cmp :: Address -> State CPU ()
cmp addr = do s@CPU{acc = av} <- get
              val <- readCPUmemory8 addr
              compare' av val

cpx :: Address -> State CPU ()
cpx addr = do s@CPU{acc = av,x = xv} <- get
              val <- readCPUmemory8 addr
              compare' xv val

cpy :: Address -> State CPU ()
cpy addr = do
  s@CPU{y = yv} <- get
  val <- readCPUmemory8 addr
  compare' yv val 

dec :: Address -> State CPU ()
dec addr = do
  s <- get
  val <-  readCPUmemory8 addr
  let (a,s') = runState (decrement val) s 
  put s' {memory = writeByte (memory s) addr a}
  return ()               

dex :: State CPU ()
dex = do
  s <- get
  let (a, s') = runState (decrement (x s)) s
  put $ s' {x = a}
  return ()

dey :: State CPU ()
dey = do s <- get
         let (a, s') = runState (decrement (y s)) s
         put $ s' {y = a}
         return ()

eor :: Address -> State CPU ()
eor addr = do
  s@CPU{acc = a, z = zv, n = nv} <- get
  y <- readCPUmemory8 addr
  let val = xor a y 
  put $ s {acc = val
          ,z = if val == 0 then 1 else zv
          ,n = if bitGet 8 val == 1 then 1 else nv}
  

inc :: Address -> State CPU ()
inc addr = do s <- get
              val <- readCPUmemory8 addr
              let (a,s') = runState (augment val) s
              put $ s' {memory = (writeByte (memory s) addr a)} 
              return ()

inx :: State CPU ()
inx = do s <- get
         let (a, s') = runState (augment (y s)) s
         put $ s' {y = a}
         return ()

iny :: State CPU ()
iny = do s <- get
         let (a, s') = runState (augment (y s)) s
         put $ s' {y = a}
         return ()

jsr :: Address -> State CPU ()
jsr addr = do CPU{pc = pcv} <- get 
              pushAddr (pcv - 1)
              jmp addr
              

jmp :: Address -> State CPU ()
jmp addr = do
  s@CPU{pc = pcv} <- get
  put $ s {pc = addr}
  return ()

lda :: Address -> State CPU ()
lda addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (acc s)) s
  put $ s' {acc = a}
  

ldx :: Address -> State CPU ()
ldx addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (x s)) s
  put $ s' {x = a}


ldy :: Address -> State CPU ()
ldy addr = do
  s <- get
  val <- readCPUmemory8 addr
  let (a, s') = runState (arithmetic (\_ -> val) (y s)) s
  put $ s' {y = a}

  
lsr :: AddressMode -> Address -> State CPU ()
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
    _           -> do put s {memory = writeByte (memory s) addr val 
,c = bit7
,v = (if bit7' == 1 then 1 else vv)}

nop :: State CPU ()
nop = return ()

ora :: Address -> State CPU ()
ora addr = do
  s@CPU{acc = a} <- get
  val <- readCPUmemory8 addr
  let (a', s') = runState (arithmetic (a.|.) (x s)) s
  put $ s' {acc = a'}
  return ()

pha :: State CPU ()
pha = do CPU{acc = a} <- get
         push a
  

php :: State CPU ()
php = do s@CPU{acc = a} <- get
         push (flagsToByte s)

pla :: State CPU ()
pla = do
  s <- get 
  let s' = execState (pop) s
  put s'
  
  return ()

sec :: State CPU ()
sec = do
  s <- get
  put $ s {c = 1}
  return ()

slo :: Address -> State CPU ()
slo addr = do
  s <- get
  let (a,s') = runState (readCPUmemory8 addr) s
      r = (shiftL a 1) .|. (acc s')
  put s' {acc = r}
  asl Immediate addr
  return ()
  

plp :: State CPU ()
plp = byteToFlags

rol :: AddressMode -> Address -> State CPU ()
rol mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `rotateL` 1
      bit7' = bitGet 8 acc'  
  case mode of
    Accumulator -> do put $ s {acc = acc'
                              ,c = bit7
                              ,v = (if bit7' == 1 then 1 else vv)}
                      return ()
    _           -> do val <- readCPUmemory8 addr
                      let val7  = bitGet 8 val
                          val'  = val `rotateL` 1
                          val7' = bitGet 8 val'
                      put $ s {memory = writeByte (memory s) addr val'
                              ,c = val7
                              ,v = (if val7' == 1 then 1 else vv)}
                      return ()

ror :: AddressMode -> Address -> State CPU ()
ror mode addr = do
  s@CPU{acc = av, v = vv} <- get
  let bit7 = bitGet 8 av 
      acc' = av `rotateR` 1
      bit7' = bitGet 8 acc'  
  case mode of
    Accumulator -> do put $ s {acc = acc'
                              ,c = bit7
                              ,v = (if bit7' == 1 then 1 else vv)}
                      return ()
    _           -> do
      val <- readCPUmemory8 addr
      put $ s {memory = writeByte (memory s) addr val
              ,c = bit7
              ,v = (if bit7' == 1 then 1 else vv)}
      return ()

rra :: Address -> State CPU ()
rra addr = do
  ror (Accumulator) addr
  adc addr

rti :: State CPU ()
rti = do  
  rts
  plp

rts :: State CPU ()
rts = do
  s@CPU{sp = spv, memory = mem} <- get
  let val = evalState (readCPUmemory16 (toWord16 (spv - 2))) s
  put s {memory = writeAddr mem (toWord16 spv) 0}
  jmp val

sbc :: Address -> State CPU ()
sbc addr = do
  s@CPU{acc = a} <- get 
  val <- readCPUmemory8 addr
  let (a',s') = runState (arithmetic ((-)a) val) s
  put $ s' {acc = a', c = if bitGet 7 a /= bitGet 7 a' then 0 else c s'  }
  return ()

sed :: State CPU ()
sed = do
  s <- get
  put $ s {d = 1}
  return ()

sei :: State CPU ()
sei = do
  s <- get
  put $ s {i = 1}
  return ()

sta :: Address -> State CPU ()
sta addr = do
  s@CPU{memory = mem,acc = a} <- get
  put $ s{memory = writeByte mem addr a}
  return ()

stx :: Address -> State CPU ()
stx addr = do
  s@CPU{memory = mem, x = xv} <- get
  put $ s{memory = writeByte mem addr xv}
  return ()

sty :: Address -> State CPU ()
sty addr = do
  s@CPU{memory = mem, y = yv} <- get
  put s{memory = writeByte mem addr yv}
  return ()  

tax :: State CPU ()
tax = do
  s@CPU{acc = a, x = xv} <- get
  let s' = execState (arithmetic (\_ -> a) xv) s
  put s' {x = a}
  return ()

tay :: State CPU ()
tay = do
  s@CPU{acc = a,y = yv} <- get
  let s' = execState (arithmetic (\_ -> a) yv) s
  put $ s' {y = a}
  return ()

tsx :: State CPU ()
tsx = do
  s@CPU{sp = spv, x = xv} <- get
  let s' = execState (arithmetic (\_ -> spv) xv) s
  put s'{x = spv}
  return ()

txa :: State CPU ()
txa = do
  s@CPU{acc = a, x = xv} <- get
  let s' = execState (arithmetic (\_ -> xv) a) s
  put $ s {acc = xv}
  return ()

txs :: State CPU ()
txs = do
  s@CPU{x = xv,sp = spv} <- get
  let s' = execState (arithmetic (\_ -> xv) spv ) s
  put $ s {sp = xv}
  return ()

tya :: State CPU ()
tya = do
  s@CPU{acc = a, y = yv} <- get
  let s' = execState (arithmetic (\_ -> yv) a) s
  put $ s {acc = yv}
  return ()


--auxilliary functions

byteToFlags :: State CPU () 
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
                                   

push :: Byte -> State CPU ()
push val = do
  s@CPU{memory = mem, sp = spv} <- get
  put $ s {memory = writeByte mem ((toWord16 spv) + 0x0100) val
          ,sp = spv + 1}
  return ()

pop :: State CPU ()
pop = do
  a@CPU{sp = spv,memory = mem} <- get
  val <- readCPUmemory8 ((toWord16 spv) - 1 + 0x0100)
  put $ a {acc = val,
           sp = spv - 1, memory = (writeByte mem (toWord16 spv -1 + 0x0100) 0)}

pushAddr :: Address -> State CPU ()
pushAddr addr = do s@CPU{memory = mem, sp = spv} <- get
                   put s {memory = writeAddr mem ((toWord16 spv) + 0x0100) addr
                         ,sp = spv +2}
                   return ()

branch :: Bool -> Address -> State CPU ()
branch cond addr = do
  s@CPU{cyclesC = cycles',pc = pcv} <- get
  val <- (readCPUmemory8 addr)
  let cycles'' = if differentPages (pc s) addr then 2 else 1
  if cond
    then do put s {pc = (fromIntegral val) + pcv , cyclesC = cycles'' + cycles'}
            return ()
    else do
    put s{cyclesC = cycles' + 2,pc = pcv + 2}
    return () 

compare' :: Byte -> Byte -> State CPU ()
compare' v1 v2 = do
  s <- get
  if v1 > v2
    then do put s {c = 1}
            return ()
    else
    if v1 == v2
    then do put s {c = 1, z = 1}
            return ()
    else do
      put s {n = 1}
      return ()

arithmetic :: (Byte -> Byte) -> Word8 -> State CPU Byte --memory arithmetic
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

augment :: Word8 -> State CPU Word8
augment val = arithmetic (+1) val

decrement :: Word8 -> State CPU Byte 
decrement val = do
  arithmetic (val-) 1 --subtraction isn't associative dingus
  
writeByte :: Memory -> Address -> Byte -> Memory
writeByte mem addr val = V.modify (\vec -> MV.write vec (fromIntegral addr) val) mem

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

step :: State CPU (Opcode,Address,Byte)
step = do
  s@CPU{memory = mem, pc = pcv} <- get
  byte <- readCPUmemory8 pcv
  let opcode@Opcode {CPU.Opcode.cycle = cycles,len = length, pageCrossCycles = extraCycles,mnem = mnemonic} = decodeOpcode byte
      op = runInstruction opcode
      (diff,address) = addressForMode s (mode opcode)
  byte' <- readCPUmemory8 (pcv + 1)
  let increasePc =
        if diff --done if not a jump or branch
        then do let s' = execState (op address) s
                put s' {cyclesC = (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles)
                                  + (fromIntegral extraCycles),
                         pc = (fromIntegral $ pc s') + (fromIntegral length)}
                return (opcode,address,byte')
        else do let s' = execState (op address) s
                put s' {cyclesC = fromIntegral (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles),
                         pc = (fromIntegral $ pc s')
                              + (fromIntegral length)}
                return (opcode,address,byte')
      samePc =
        if diff --done if not a jump or branch
        then do let s' = execState (op address) s
                put s' {cyclesC = (fromIntegral $ cyclesC s')
                                  + (fromIntegral cycles)
                                  + (fromIntegral extraCycles),
                         pc = (fromIntegral $ pc s')
                              + (fromIntegral length)}
                return (opcode,address,byte')
        else do let s' = execState (op address) s
                put s' {cyclesC = (cyclesC s')
                                  + (fromIntegral cycles)}
                return (opcode,address,byte') 
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
     

runInstruction :: Opcode -> (Address -> State CPU ())
runInstruction (Opcode _ mnemonic mode _ _ _) = case mnemonic of
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
  
  
  
  
  
  
  
  
  
  
  
