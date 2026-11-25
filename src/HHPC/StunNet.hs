module HHPC.StunNet where
import Network.Socket
import Control.Exception (try)
import HHPC.Error
import Control.Exception.Base (IOException)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL 
import Data.Binary.Put
import Crypto.Random (getRandomBytes)
import Data.Binary (Word32)
import Data.Word (Word16)
import Data.Binary.Get (Get, getWord16be, getWord32be, getByteString, runGet)
import Control.Exception (throwIO)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (when)
import Data.Binary (Word8)
import Data.Binary (Word64)

{-# INLINE stunHeaderByteSize #-}
stunHeaderByteSize :: Int
stunHeaderByteSize  = 20 

{-# INLINE transactionIdByteSize #-}
transactionIdByteSize :: Int
transactionIdByteSize  = 12

{-# INLINE transactionIdBitSize #-}
transactionIdBitSize :: Int
transactionIdBitSize  = 96

{-# INLINE attributeTypeLengthByteSize #-}
attributeTypeLengthByteSize :: Int
attributeTypeLengthByteSize = 32


{-# INLINE magicCookie #-}
magicCookie :: Word32 
magicCookie  = 0x2112A442


{-# INLINE bindRequestType #-}
bindRequestType :: Word16
bindRequestType  = 0x0001

{-# INLINE bindResponseType #-}
bindResponseType :: Word16
bindResponseType  = 0x0101

{-#INLINE bindRequestLength #-}
bindRequestLength :: Word16
bindRequestLength  = 0x0


data MappedAddr = MappedAddr
  { mappedIPFamily :: Word8
  , mappedPort     :: Word16
  , mappedAddr     :: (Word64, Word64)
  }
  deriving (Show)
  
newtype XORMappedAddr = XORMappedAddr MappedAddr 
  deriving (Show)
newtype Username = Username BS.ByteString
  deriving (Show)
-- The following is the operation that the client will perform to hash
-- the username:
-- userhash = SHA-256(OpaqueString(username) ":" OpaqueString(realm))
newtype Userhash = Userhash BS.ByteString
  deriving (Show) 
-- TODO: refer to https://datatracker.ietf.org/doc/html/rfc8489#section-14.5 onward regarding the
-- specifics of implementation. 
newtype MessageIntegrity       = MessageIntegrity       Word32
  deriving (Show)
newtype MessageIntegritySHA256 = MessageIntegritySHA256 Word32
  deriving (Show)
newtype StunFingerprint        = StunFingerprint        Word32
  deriving (Show)

data ErrorCode = ErrorCode
  { errorClass   :: Word8 -- "The Class represents the hundreds digit of the error code... value must be between 1 & 3. 3 bit field. "
  , number       :: Word8 -- "The Number represents 10 and 1s ditgits, and its value MUST be between 0 and 99."
  , reasonPhrase :: BS.ByteString 
  }
  deriving (Show)

-- Presence of the REALM attribute in a request indicates that long-term
-- credentials are being used for authentication.  Presence in certain
-- error responses indicates that the server wishes the client to use a
-- long-term credential in that realm for authentication.
newtype Realm  = Realm BS.ByteString
  deriving (Show)
newtype Nounce = Nounce BS.ByteString
 deriving (Show)

data PasswordAlgorithms = PasswordAlgorithms
  {
    
  }





-- data StunAttributes = StunAttributes
--   { mappedAddrs         :: Maybe [MappedAddr]
--   , xorMappedAddrs      :: Maybe [XORMappedAddr]
--   , messageIntegrity    :: Maybe MessageIntegrity
--   , messageIntegrity256 :: Maybe MessageIntegritySHA256
--   , errorCode           :: Maybe ErrorCode
--   , stunFingerprint     :: Maybe StunFingerprint
--   , realm               :: Maybe Realm
--   , nounce              :: Maybe Nounce
--   , pwdAlgorithms       :: Maybe [PasswordAlgorithms]
   
--   }

data StunAttribute a
  = MkMappedAddr          [MappedAddr]
  | MkXorMappedAddrs      [XORMappedAddr]
  | MkMessageIntegrity    MessageIntegrity
  | MkMessageIntegrity256 MessageIntegritySHA256
  | MkErrorCode           ErrorCode
  | MkStunFingerprint     StunFingerprint
  | MkRealm               Realm
  | MkNounce              Nounce
  | MkPwdAlgorithms       [PasswordAlgorithms]
   
  


hints :: AddrInfo
hints = defaultHints
 { addrFamily = AF_INET
 , addrSocketType = Datagram 
 }

stunServerNames :: [HostName]
stunServerNames =
  [ "stun1.l.google.com",
    "stun2.l.google.com",
    "stun3.l.google.com",
    "stun4.l.google.com"
  ]

-- TODO: exmaine error handling again 
resolve :: [HostName] -> IO [AddrInfo]
resolve [] = throwIO $ userError "Failed to resolve STUN server domain names." 
resolve (x:xs) = do
  result <- try (getAddrInfo (Just hints) (Just x) (Just "19302")) :: IO (Either IOException  [AddrInfo]) 
  case result of 
    Left  _ -> resolve xs
    Right r -> return r               
      
resolveStunAddrs :: IO AddrInfo
resolveStunAddrs = do
  addr <- resolve stunServerNames  
  case (listToMaybe addr) of
      Nothing -> throwIO impossibleError 
      Just a -> return a


generateTransactionId :: IO BS.ByteString
generateTransactionId = getRandomBytes transactionIdByteSize


-- mandatory range: 0x0000 -> 0x7FFF 
-- optional range:  0x8000 -> 0xFFFF


stunRequest :: BS.ByteString -> BS.ByteString
stunRequest tid = BS.toStrict . runPut $ do
  putWord16be bindRequestType
  putWord16be bindRequestLength
  putWord32be magicCookie
  putByteString tid

stunHeaderParser :: Get (Word16, Word16, Word32, BS.ByteString)
stunHeaderParser = do
  mtype <- getWord16be
  len   <- getWord16be
  ckie  <- getWord32be
  tid   <- getByteString transactionIdByteSize
  return (mtype, len, ckie, tid)



type TypeLength = (Word16, Word16)
stunAttributeTLParser :: Get TypeLength 
stunAttributeTLParser = do
  atype <- getWord16be
  alen  <- getWord16be
  return (atype, alen) 

type Value      = BS.ByteString 
stunAttributeVParser :: Word16 -> Get Value 
stunAttributeVParser len = getByteString $ fromIntegral len -- TODO: do something if a returned length is >Word16

parseTLVs :: Socket -> Word16 -> IO [a]
parseTLVs sock totalLen = p 0 [] sock totalLen
  where
    p :: Word16 -> [a] -> Socket -> Word16 -> IO [a]
    p bytesRead accum sock target = do    
      tlresponse <- recv sock attributeTypeLengthByteSize
      let (atype, vlen) = runGet stunAttributeTLParser $ BSL.fromStrict tlresponse
      vresponse <- recv sock $ fromIntegral vlen 
      let value = runGet (stunAttributeVParser vlen) $ BSL.fromStrict vresponse
          currentRead = attributeTypeLengthByteSize + fromIntegral vlen
      if currentRead == fromIntegral target then undefined
      else undefined   

mkStunRequest :: IO [StunAttribute a]
mkStunRequest = do
  addr <- resolveStunAddrs
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  tid <- generateTransactionId
  connect sock $ addrAddress addr
  sendAll sock $ stunRequest tid 

  hresponse <- recv sock stunHeaderByteSize
  let (mtype, totalLength, ckie, rtid) = runGet stunHeaderParser $ BSL.fromStrict hresponse
  when (ckie /= magicCookie || rtid /= tid) $ throwIO $ userError "Bad request, transaction id or cookie is incorrect"
  when (mtype /= bindResponseType) $ throwIO $ userError "Bad request, message type is invalid"

   
  


   
