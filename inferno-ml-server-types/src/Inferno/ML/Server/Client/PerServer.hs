-- | @ClientM@ functions for setting\/getting the per-server configuration for
-- the @inferno-ml-server@ instance
module Inferno.ML.Server.Client.PerServer where

import Inferno.ML.Server.Types.PerServer
import Servant ((:<|>) ((:<|>)))
import Servant.Client (ClientM, client)
import Data.Data (Proxy (Proxy))

-- | Set the current per-server config for the associated @inferno-ml-server@
-- instance. This will overwrite any existing configuration; consider using
-- 'getPerServerConfigC' to first get and then update fields as necessary
setPerServerConfigC :: PerServerConfig -> ClientM ()

-- | Get any per-server configuration for the associated @inferno-ml-server@
-- instance; if no configuration has been set, an exception is raised
getPerServerConfigC :: ClientM PerServerConfig

setPerServerConfigC :<|> getPerServerConfigC = client api

api :: Proxy PerServerAPI
api = Proxy
