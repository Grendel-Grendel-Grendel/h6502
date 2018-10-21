module Main where

import Control.Monad.STM
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Vector.Unboxed (toList,slice,indexed)
import System.IO (stdin,hReady)
import Data.Word
import Numeric (showHex,readHex)
import CPU.Types
import CPU

main :: IO ()
main = do
  rom <- getRom "/home/nomad/test"
  let s = execState (loadMemory rom (0)) initCPU
  putStrLn "enter address where program should pause"
  initSteps <- getLine --gets a number which when the pc == pauses the stepping.  
  runLoop s $ (fromIntegral . fst . head) $ readHex initSteps
  putStrLn "finished"
  return ()

runLoop :: CPU -> Word16 -> IO ()
runLoop s iter =
  if pc s == iter
  then do
    putStrLn "enter steps or press enter to exit with stack printed"
    steps <- getLine
    case steps of
      "" -> showStack (memory s)
      x  -> runLoop s $ (fromIntegral . fst . head) $ readHex x
  else
    do 
      let ((a,addr,val,ioFunction),s') = runState (step) s 
      putStrLn $ show a
      putStrLn $
        "pc: " ++ (showHex (pc s) "") ++
        " |sp: " ++ (showHex (sp s')"") ++
        " |x: " ++ (showHex (x s')"") ++
        " |y: " ++ (showHex (y s')"") ++
        " |acc: " ++ (showHex (acc s')"") ++
        " |addr: " ++ ((showHex addr)"") ++
        " |val: " ++ ((showHex val)"") ++ --value at above addr
        " |z: " ++ ((showHex (z s'))"") ++
        " |n: " ++ ((showHex (n s'))"") ++
        " |c: " ++ ((showHex (c s'))"") ++
        " |v: " ++ ((showHex (c s'))"")
      keyPressed <- hReady stdin
      case ioFunction of
        Just f -> f
        Nothing -> return ()
      if keyPressed then showStack (memory s') else runLoop s' iter
  where showStack mem =
          print $ fmap showHexTuple $ (toList . slice 0x100 0xFF) $ indexed mem
        showHexTuple (x,y) = (showHex x "",showHex y "")
        showHex' x = showHex x ""
