{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Config
    ( module DomainDriven.Config
    , Name
    ) where

import           Language.Haskell.TH
import           Prelude
import           DomainDriven.Internal.Class
import           DomainDriven.Internal.HasFieldName
import qualified Data.Map                                     as M
import           Data.Text                      ( Text )
import           Data.Maybe


-- | Configuration used to generate server
-- This is expected to be generated by `mkServerConfig`. It is only explicit due to
-- the GHC stage restrictions.
data ServerConfig = ServerConfig
    { allApiOptions :: M.Map String ApiOptions
        -- ^ Map of API options for all action GADTs used in the API
    , allFieldNames :: M.Map String Text
        -- ^ Map of field names for all types used as captures
    }
    deriving Show

configName :: Name
configName = mkName "domaindrivenServerConfig"

mkServerConfig :: Q [Dec]
mkServerConfig = do
    [d| $(varP configName) = ServerConfig $(getApiOptionsMap) $(getFieldNameMap)   |]

-- | Generates `Map String ApiOptions`
-- Containing the ApiOptions of all types with an ApiOpts instance
getApiOptionsMap :: Q Exp
getApiOptionsMap = reify ''HasApiOptions >>= \case
    ClassI _ instances -> do
        cfgs <- traverse nameAndCfg instances

        [e| M.fromList  $(pure $ ListE cfgs) |]
    i -> fail $ "Expected ClassI but got: " <> show i
  where
    nameAndCfg :: Dec -> Q Exp
    nameAndCfg = \case
        InstanceD _ _ (AppT _ ty@(ConT n)) _ ->
             [e| ( $(stringE $ nameBase n), apiOptions @($(pure ty)))|]
        d -> fail $ "Expected instance InstanceD but got: " <> show d


-- | Generates `Map String Text`
getFieldNameMap :: Q Exp
getFieldNameMap = reify ''HasFieldName >>= \case
    ClassI _ instances -> do
        cfgs <- catMaybes <$> traverse typeAndFieldName instances
        [e| M.fromList $(pure $ ListE cfgs) |]
    i -> fail $ "Expected ClassI but got: " <> show i
  where
    typeAndFieldName :: Dec -> Q (Maybe Exp)
    typeAndFieldName = \case
        InstanceD _ _ (AppT _ ty@(ConT n)) _ ->
            Just <$> [e| ($(stringE $ nameBase n), fieldName @($(pure ty)))|]
        InstanceD _ _ _ _ -> pure Nothing
        d                 -> fail $ "Expected instance InstanceD but got: " <> show d
