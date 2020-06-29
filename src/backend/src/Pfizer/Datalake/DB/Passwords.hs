module Pfizer.Datalake.DB.Passwords (
    HashedPassword(..)
  , hashPassword
  , validatePassword
  , hashedPasswordToString
  , hashedPasswordFromString
  ) where

import qualified Crypto.BCrypt         as BCrypt
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8  as BS.UTF8

import Pfizer.Datalake.Interface

-- | Hashed password (including salt)
newtype HashedPassword = HashedPassword BS.ByteString
  deriving (Show, Eq, Ord)

hashPassword :: PlaintextPassword -> IO HashedPassword
hashPassword (PlaintextPassword password) = do
    Just hash <- BCrypt.hashPasswordUsingPolicy
                   BCrypt.slowerBcryptHashingPolicy
                   (BS.UTF8.fromString password)
    return $ HashedPassword hash

validatePassword :: HashedPassword -> PlaintextPassword -> Bool
validatePassword (HashedPassword hash) (PlaintextPassword password) =
    BCrypt.validatePassword hash (BS.UTF8.fromString password)

hashedPasswordToString :: HashedPassword -> String
hashedPasswordToString (HashedPassword hash) = BS.C8.unpack hash

hashedPasswordFromString :: String -> HashedPassword
hashedPasswordFromString hash = HashedPassword (BS.C8.pack hash)
