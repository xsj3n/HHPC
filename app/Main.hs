module Main (main) where
import HHPC.StunNet
import System.Exit (exitFailure)
import Data.Maybe (isNothing)
import Control.Monad (when)


-- 1. gather ip:port via stun
-- 2. both clients check a signal server 

main :: IO ()
main = do
  addr <- resolveStunAddrs
  when (isNothing addr) $ print "[!] Unable to resolve google stun server domain names, exiting." >> exitFailure
  
