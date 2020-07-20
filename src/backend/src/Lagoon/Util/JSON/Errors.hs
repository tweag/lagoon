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
module Lagoon.Util.JSON.Errors (
    JsonError(..)
  , throwOnError
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Conduit

newtype JsonError = JsonError String
  deriving (Show)

instance Exception JsonError

-- | Throw exception when we get a JSON error
--
-- TODO: In the future it might be nicer to make an attempt to recover from
-- JSON errors.
throwOnError :: MonadIO m => Conduit (Either JsonError a) m a
throwOnError = awaitForever $ \case
                 Left err -> liftIO $ throwIO err
                 Right a  -> yield a
