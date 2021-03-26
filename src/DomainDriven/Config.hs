{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Config
    ( module DomainDriven.Config
    , Name
    ) where

import           Language.Haskell.TH
import           Prelude
import           DomainDriven.Internal.Class
import qualified Data.Map                                     as M


-- | Generates `Map Name ApiOptions`
-- Containing the ApiOptions of all types with an ApiOpts instance
getApiOptionsMap :: Q Exp
getApiOptionsMap = reify ''HasApiOptions >>= \case
    ClassI _ instances -> do
        cfgs <- traverse nameAndCfg instances

        [e| M.fromList $ (\(k, v) -> (mkName k, v)) <$> $(pure $ ListE cfgs) |]
    i -> fail $ "Expected ClassI but got: " <> show i
  where
    nameAndCfg :: Dec -> Q Exp
    nameAndCfg = \case
        InstanceD _ _ (AppT _ ty@(ConT n)) _ ->
             [e| ( $(stringE $ show n), apiOptions @($(pure ty)))|]
        d -> fail $ "Expected instance InstanceD but got: " <> show d
