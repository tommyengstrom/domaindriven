module DomainDriven.Server where

import           DomainDriven.Internal.Class



-- The first goal is to generate a server from a `CmdHandler model event cmd err`. Later
-- on I will refactor queris to alsu use a GADT and follow the same pattern.
--
-- In order to achieve this I will
-- * Start by writing the template haskell to generate endpoint types from a `Cmd a`
-- * All arguments will be encoded in the request body
-- * All commands use POST or PUT (does one make more sense?)
-- * Ensure that error messages are easy to understand!
--
--
