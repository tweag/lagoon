-- Copyright 2020 Pfizer Inc.

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     https://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
module Lagoon.DB.Passwords (
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

import Lagoon.Interface

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
