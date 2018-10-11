module Main where

import Control.Monad.STM
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar
import Numeric (showHex)
import CPU.Types
import CPU

main :: IO ()
main = do
  rom <- getRom "/home/nomad/test"
  let s = execState (loadMemory rom 0x400) initCPU
  runLoop s
  putStrLn "finished"
  return ()

runLoop :: CPU -> IO ()
runLoop s = do
  let ((a,addr,val,ioFunction),s') = runState (step) s
  putStrLn $ show a
  putStrLn $ "pc: " ++ (showHex (pc s) "") ++
             " sp: " ++ (showHex (sp s')"") ++
             " x: " ++ (showHex (x s')"") ++
             " y: " ++ (showHex (y s')"") ++
             " acc: " ++ (showHex (acc s')"") ++
             " addr: " ++ ((showHex addr)"") ++
             " val: " ++ ((showHex val)"") ++ --value at above addr
             " z: " ++ ((showHex (z s'))"")
  case ioFunction of
    Just f -> f
    Nothing -> return ()
  threadDelay 20000
  runLoop s'
  
