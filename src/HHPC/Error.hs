module HHPC.Error where
import Control.Exception (Exception)
import Control.Exception.Base

impossibleError :: IOException
impossibleError = userError "This error should be impossible to encounter."

printResolutionError :: (Exception e) => e -> IO ()
printResolutionError err = print $ "[!] Unable to resolve STUN server name: " ++ show err

