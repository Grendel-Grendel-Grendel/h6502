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
         pushAddr (pc s + 2)
         php
         s' <- get
         put s' {i = 1}
         readCPUmemory16 0xFFFE >>= jmp 

adc :: Address -> State CPU IOevent
adc addr = do
  s@CPU{acc = av,c = cv} <- get 
  val <- readCPUmemory8 addr
  let av' = val + av + cv
      doesOverflow = ((av`xor`val) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
      shouldCarry = toInt av + toInt val + toInt cv > 0xFF
  put $ s {acc = av', c = if shouldCarry then 1 else 0
          ,v = if doesOverflow then 1 else 0}
  checkValFlags av'

ahx :: Address -> State CPU IOevent
ahx addr = do
  s@CPU{acc = av, x = xv} <- get
  let val' = av .&. xv .&. ((fst $ splitW16 addr) + 1)
  writeByte addr val'

and :: Address -> State CPU IOevent
and addr = do
  s@CPU{acc = av} <- get
  val <- readCPUmemory8 addr
  put $ s {acc = av .&. val}
  checkValFlags (av .&. val)

tas :: Address -> State CPU IOevent
tas addr = do
  s@CPU{acc = av, x = xv} <- get
  let val = av .&. xv
  put s {sp = val}
  ahx addr

shx :: Address -> State CPU IOevent
shx addr = do
  CPU{x = xv, acc = av} <- get
  writeByte addr (xv .&. av)

shy :: Address -> State CPU IOevent
shy addr = do
  CPU{y = yv, acc = av} <- get
  writeByte addr (yv .&. av)

las :: Address -> State CPU IOevent
las addr = do
  s@CPU{sp = spv} <- get
  val <- readCPUmemory8 addr 
  let a  = spv .&. val
  put s {acc = a, x = a, y = a}
  checkValFlags a 
  return Nothing
  

asl :: AddressMode -> Address -> State CPU IOevent
asl mode addr = do
  s@CPU{acc = av} <- get  
  case mode of
    Accumulator -> do let bit7 = bitGet 7 av 
                          acc' = av `shiftL` 1
                          bit7' = bitGet 7 acc'
                      put s {acc = acc'
                            ,c = bit7
                            ,n = bit7'
                            ,z = if acc' == 0 then 1 else 0}
                      return Nothing
    _           -> do val <- readCPUmemory8 addr
                      let bit7v  = bitGet 7 val 
                          val'   = val `shiftL` 1
                          bit7v' = bitGet 7 val'
                      put s {c = bit7v
                            ,n = bit7v'
                            ,z = if val' == 0 then 1 else 0}
                      writeByte addr val'
        
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
  let bit7 = bitGet 7 val
      bit6 = bitGet 6 val
  checkValFlags (val .&. av) --Must be done in this order here so put overrides
  s' <- get
  put s' {n = bit7, v = bit6}
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
  let a = val - 1
  checkValFlags a
  writeByte addr a

dcp :: Address -> State CPU IOevent
dcp address = do
  dec address
  cmp address

dex :: State CPU IOevent
dex = do
  s <- get
  let a = (x s) - 1 
  put $ s {x = a}
  checkValFlags a

dey :: State CPU IOevent
dey = do s <- get
         let a = (y s) - 1 
         put $ s {y = a}
         checkValFlags a

eor :: Address -> State CPU IOevent
eor addr = do
  s@CPU{acc = a, z = zv, n = nv} <- get
  y <- readCPUmemory8 addr
  let val = xor a y 
  put $ s {acc = val}
  checkValFlags val

inc :: Address -> State CPU IOevent
inc addr = do val <- readCPUmemory8 addr
              checkValFlags (val + 1)
              writeByte addr (val + 1)
              

inx :: State CPU IOevent
inx = do s <- get
         let a = (x s) + 1
         put $ s {x = a}
         checkValFlags a

iny :: State CPU IOevent
iny = do s <- get
         let a = ((y s) + 1)
         put $ s {y = a}
         checkValFlags a

jsr :: Address -> State CPU IOevent
jsr addr = do CPU{pc = pcv} <- get 
              pushAddr (pcv+2)
              jmp addr
              

jmp :: Address -> State CPU IOevent
jmp addr = do
  s <- get
  put $ s {pc = addr}
  return Nothing

lda :: Address -> State CPU IOevent
lda addr = do
  s <- get
  val <- readCPUmemory8 addr
  put s {acc = val}
  checkValFlags val

ldx :: Address -> State CPU IOevent
ldx addr = do
  s <- get
  val <- readCPUmemory8 addr
  put $ s {x = val}
  checkValFlags val

ldy :: Address -> State CPU IOevent
ldy addr = do
  s <- get
  val <- readCPUmemory8 addr
  put $ s {y = val}
  checkValFlags val
  
lsr :: AddressMode -> Address -> State CPU IOevent
lsr mode addr = do
  s@CPU{acc = av, n = nv,z = zv} <- get
  case mode of
    Accumulator -> do  let bit0 = bitGet 0 av 
                           acc' = av `shiftR` 1
                           bit7' = bitGet 7 acc'
                       put s {acc = acc'
                            ,c = bit0
                            ,n = bit7'
                            ,z = if acc' == 0 then 1 else 0}
                       return Nothing
    _           -> do
      val <- readCPUmemory8 addr
      let bit0v  = bitGet 0 val 
          val'   = val `shiftR` 1
          bit7v' = bitGet 7 val'
      put $ s {c = bit0v
              ,n = bit7v'
              ,z = if val' == 0 then 1 else 0}
      writeByte addr val'

sre :: Address -> State CPU IOevent
sre addr = do
  s <- get
  val <- readCPUmemory8 addr
  let a  = val `shiftR` 1
      a' = xor a (acc s)
  lsr Immediate addr 
  put s {acc = a'}
  checkValFlags a'

nop :: State CPU IOevent
nop = return Nothing

ora :: Address -> State CPU IOevent
ora addr = do
  s@CPU{acc = a} <- get
  val <- readCPUmemory8 addr
  let a' = a .|. val
  put $ s {acc = a'}
  checkValFlags a'
  return Nothing

pha :: State CPU IOevent
pha = do s <- get
         push (acc s)
  

php :: State CPU IOevent
php = do s <- get
         push (flagsToByte $ s)

pla :: State CPU IOevent
pla = do
  pop
  return Nothing

sec :: State CPU IOevent
sec = do
  s <- get
  put $ s {c = 1}
  return Nothing

rla :: Address -> State CPU IOevent
rla addr = do
  s <- get
  let (a,s') = runState (readCPUmemory8 addr) s
      r = (shiftR a 1) .|. (acc s')
  put s' {acc = r}
  asl Immediate addr
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
  s@CPU{acc = av,c = cv} <- get  
  case mode of
    Accumulator -> do let bit7 = bitGet 7 av 
                          av' = (av `shiftL` 1) .|. cv
                          bit7' = bitGet 7 av'
                      put $ s {acc = av'
                              ,c = bit7
                              ,n = bit7'
                              ,z = if av' == 0 then 1 else 0}
                      return Nothing
    _           -> do val <- readCPUmemory8 addr
                      let val7  = bitGet 7 val
                          val'  = (val `shiftL` 1) .|. cv
                          val7' = bitGet 7 val'
                      put s{c = val7
                           ,n = val7'
                           ,z = if val' == 0 then 1 else 0}
                      writeByte addr val'

ror :: AddressMode -> Address -> State CPU IOevent
ror mode addr = do
 s@CPU{acc = av,c = cv} <- get  
 case mode of
   Accumulator -> do let bit0 = bitGet 0 av 
                         av' = (av `shiftR` 1) .|. (shiftL cv 7)
                         bit7 = bitGet 7 av'
                     put $ s {acc = av'
                             ,c = bit0
                             ,n = bit7
                             ,z = if av' == 0 then 1 else 0}
                     return Nothing
   _           -> do val <- readCPUmemory8 addr
                     let val0  = bitGet 0 val
                         val'  = val `shiftR` 1 .|. (shiftL cv 7)
                         val7 = bitGet 7 val'
                     put s{c = val0
                          ,n = val7
                          ,z = if val' == 0 then 1 else 0}
                     writeByte addr val'

rra :: Address -> State CPU IOevent
rra addr = do
  ror (Immediate) addr
  adc addr

rti :: State CPU IOevent
rti = do  
  plp
  rts

rts :: State CPU IOevent
rts = do
  s@CPU{sp = spv, memory = mem} <- get
  val <- pullAddr ((fromIntegral spv) + 0x100 + 2)
  put s {memory = writeAddr mem ((toWord16 spv) + 0x0100 + 2) 0, sp = spv + 2}
  jmp val

sbc :: Address -> State CPU IOevent
sbc addr = do
  s@CPU{acc = av} <- get 
  val <- readCPUmemory8 addr
  let av' = av - val - (1 - c s)
      doesOverflow = ((av `xor` val) .&. 0x80) /= 0 &&
                     ((av `xor` av') .&. 0x80) /= 0
      shouldCarry = toInt av - toInt val - toInt (1 - c s) >= 0
  put $ s {acc = av', c = if shouldCarry then 1 else 0 
          ,v = if doesOverflow then 1 else 0}
  checkValFlags av'
  
isc :: Address -> State CPU IOevent
isc addr = do
  inc addr
  sbc addr

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
  CPU{acc = a} <- get
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
  put s {x = a}
  checkValFlags a

tay :: State CPU IOevent
tay = do
  s@CPU{acc = a,y = yv} <- get
  put $ s {y = a}
  checkValFlags a

tsx :: State CPU IOevent
tsx = do
  s@CPU{sp = spv, x = xv} <- get
  put s{x = spv}
  checkValFlags spv

txa :: State CPU IOevent
txa = do
  s@CPU{acc = a, x = xv} <- get
  put $ s {acc = xv}
  checkValFlags xv

txs :: State CPU IOevent
txs = do
  s@CPU{x = xv} <- get
  put $ s {sp = xv}
  return Nothing

tya :: State CPU IOevent
tya = do
  s@CPU{acc = a, y = yv} <- get
  put $ s {acc = yv}
  checkValFlags yv


--auxilliary functions

byteToFlags :: State CPU IOevent 
byteToFlags = do
  s@CPU{sp = spv} <- get
  bt <- readCPUmemory8 ((toWord16 (spv + 1))+0x0100)
  writeByte ((toWord16 (spv+1)) + 0x0100) 0x0
  put s {c = bitGet 0 bt
        ,z = bitGet 1 bt
        ,i = bitGet 2 bt
        ,d = bitGet 3 bt
        ,b = bitGet 4 bt
        ,v = bitGet 6 bt 
        ,n = bitGet 7 bt
        ,sp = spv + 0x1
        }
  return Nothing

push :: Byte -> State CPU IOevent
push val = do
  s@CPU{memory = mem, sp = spv} <- get
  writeByte ((toWord16 spv) + 0x0100) val
  s'' <- get
  put s'' { sp = spv - 1 }
  return Nothing

pop :: State CPU IOevent
pop = do
  a@CPU{sp = spv,memory = mem} <- get
  val <- readCPUmemory8 ((toWord16 (spv + 1)) + 0x0100) 
  put $ a {acc = val
          ,sp = spv + 1
          ,n = if bitGet 7 val == 1 then 1 else 0
          ,z = if val == 0 then 1 else 0}
  writeByte (toWord16 spv + 0x0100) 0
  return Nothing

pushAddr :: Address -> State CPU IOevent
pushAddr addr = do s@CPU{memory = mem, sp = spv} <- get
                   put s {memory = writeAddr mem ((toWord16 spv) + 0x0100) addr
                         ,sp = spv - 2}
                   return Nothing

branch :: Bool -> Address -> State CPU IOevent
branch cond addr = do
  s@CPU{cyclesC = cycles',pc = pcv} <- get
  let cycles'' = if differentPages pcv addr then 2 else 1
  if cond
    then do put s {pc = addr, cyclesC = cycles'' + cycles'}
            return Nothing
    else do
    put s{cyclesC = cycles' + 2,pc = pcv + 2}
    return Nothing

compare' :: Byte -> Byte -> State CPU IOevent
compare' v1 v2 = do
  s <- get
  let bit7 = bitGet 7 (v1 - v2)
  if v1 > v2
    then do put s {c = 1,z = 0,n = bit7}
            return Nothing
    else
    if v1 == v2
    then do put s {c = 1,z = 1,n = 0}
            return Nothing
    else do
      put s {n = bit7,c = 0, z = 0}
      return Nothing

checkOverflow :: Byte -> Byte -> Byte -> State CPU IOevent
checkOverflow av bv r = do
  s <- get
  let bool = ((av `xor` bv) .&. 0x80) == 0 &&
             ((av `xor` r) .&. 0x80) /= 0
  if bool
    then do put s{v = 1}
            return Nothing
    else do put s{v = 0}
            return Nothing
            
  
checkValFlags :: Byte -> State CPU IOevent
checkValFlags val = do
  s <- get
  let nv = bitGet 7 val
  let zv = if val == 0 then 1 else 0
  put s {n = nv, z = zv}
  return Nothing
    
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
    MV.write vector (fromIntegral target) hi
    MV.write vector (fromIntegral $ target - 1) lo -- must be descending because only used for the stack
    V.unsafeFreeze vector
  where (lo,hi) = splitW16 val

readCPUmemory8 :: Address -> State CPU Byte
readCPUmemory8 i = do
  CPU{memory = mem} <- get
  return $ mem V.! (fromIntegral i)

--reads an address in ascending fashion
readCPUmemory16 :: Address -> State CPU Address
readCPUmemory16 i = do
  lo <- readCPUmemory8 i 
  hi <- readCPUmemory8 (i+1)
  return (makeW16 lo hi)

--reads an address in descending fashion
pullAddr :: Address -> State CPU Address
pullAddr i = do
  hi <- readCPUmemory8 i 
  lo <- readCPUmemory8 (i-1)
  return (makeW16 lo hi)

read16Bug :: Address -> State CPU Address
read16Bug addr = do
  lo <- readCPUmemory8 addr
  hi <- readCPUmemory8 $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
  return $  makeW16 lo hi

differentPages :: Word16 -> Word16 -> Bool
differentPages a b = (a .&. 0xFF00) /= (b .&. 0xFF00)

addressForMode :: CPU -> AddressMode -> (Bool, Address)
addressForMode s@CPU {memory = mem, pc = pcv, x = xv, y = yv} mode =
  case mode of
    Implied -> (False, 0)
    Accumulator -> (False, 0)
    Immediate   -> (False, pcv+1)
    ZeroPage    -> (False, toWord16 immByte)
    ZeroPageX   -> (False, toWord16 (immByte + xv))
    ZeroPageY   -> (False, toWord16 (immByte + yv))
    Relative    -> let offset16 = immAddr -- already does branching offset math!
                       offset8  = firstNibble offset16
                       diff = if offset8 < 0x80 then 0 else 0x100
                   in (False, pcv + 2 + offset8 - diff)
    Absolute    -> (False, immAddr)
    AbsoluteX   -> let addr = immAddr + toWord16 xv
                       different = differentPages immAddr addr
                   in (different, addr)
    AbsoluteY   -> let addr = immAddr + toWord16 yv
                       different = differentPages immAddr addr
                   in (different, addr)
    Indirect    -> (False, evalState (read16Bug immAddr) s)
    IndexedIndirect -> (False, evalState (read16Bug (toWord16 $ immByte + xv))s)
    IndirectIndexed -> let addr = (evalState (read16Bug indAddr)s) + toWord16 yv
                           different = differentPages immAddr addr 
                       in (different,addr)
  --immediate addr and byte
  where immAddr = evalState (readCPUmemory16 (pcv+1)) s
        immByte = evalState (readCPUmemory8  (pcv+1)) s
        indAddr = toWord16 immByte

step :: State CPU (Opcode,Address,Byte,IOevent)
step = do
  s@CPU{memory = mem, pc = pcv} <- get
  byte <- readCPUmemory8 pcv
  let opcode@Opcode {CPU.Opcode.cycle = cycles,len = length, pageCrossCycles = extraCycles,mnem = mnemonic} = decodeOpcode byte
      op = getInstruction opcode
      (diff,address) = addressForMode s (mode opcode)
  byte' <- readCPUmemory8 address
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
    BCS -> samePc
    BCC -> samePc
    BMI -> samePc
    BPL -> samePc
    BVC -> samePc
    BVS -> samePc
    JMP -> samePc
    BRK -> samePc
    JSR -> samePc
    RTI -> samePc
    _   -> increasePc

getInstruction :: Opcode -> (Address -> State CPU IOevent)
getInstruction (Opcode _ mnemonic mode _ _ _) = case mnemonic of
  ADC -> adc
  AHX -> ahx
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
  DCP -> dcp
  DEX -> const dex
  DEY -> const dey
  EOR -> eor
  INC -> inc
  ISC -> isc
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
  RLA -> rla
  RTI -> const rti
  RTS -> const rts
  ROR -> ror mode
  ROL -> rol mode
  RRA -> rra
  SBC -> sbc
  SEC -> const sec
  SED -> const sed
  SEI -> const sei
  SHX -> shx
  SHY -> shy
  SLO -> slo
  SRE -> sre
  STA -> sta
  STX -> stx
  STY -> sty
  TAS -> tas
  LAS -> las
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
  
  
