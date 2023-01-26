{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.ServerSpec where

import Action.ExtraParam
import Action.ServerTest
import Action.Store
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch (try)
import Control.Monad.Except
import Data.Map qualified as M
import Data.Text (Text)
import DomainDriven
import DomainDriven.Internal.NamedFields
import DomainDriven.Persistance.ForgetfulInMemory
import DomainDriven.Server.Config (ServerConfig (..))
import Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    )
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Hspec
import Prelude

import Data.Kind (Type)
import UnliftIO (MonadUnliftIO)

$(mkServer extraParamConfig ''ExtraParamAction)

-----------------------------------------------------------------------------------------
-- type ExtraParamActionApi (ep_idrrs :: ExtraP) (bool_idrrr :: Bool) =
--    (:<|>) ExtraParamAction_ReverseTextEndpoint ((:<|>) ((:>) "Sub1" (ExtraParamAction_Sub1Api ep_idrrs)) ((:<|>) ((:>) "Sub2" ExtraParamAction_Sub2Api) ((:>) "Sub3" (ExtraParamAction_Sub3Api ep_idrrs bool_idrrr))))
-- type ExtraParamAction_ReverseTextEndpoint =
--    (:>) "ReverseText" ((:>) (ReqBody '[JSON] (NF1 "ExtraParamAction_ReverseText" "text" Text)) (Verb 'POST 200 ( '(:) JSON ('[] :: [Type])) Text))
-- type ExtraParamAction_Sub1Api (ep_idrrs :: ExtraP) =
--    (:<|>) ExtraParamAction_Sub1_TalkToMeEndpoint (ExtraParamAction_Sub1_SayMyNumberEndpoint ep_idrrs)
-- type ExtraParamAction_Sub1_TalkToMeEndpoint =
--    (:>) "TalkToMe" (Verb 'GET 200 ( '(:) JSON ('[] :: [Type])) Text)
-- type ExtraParamAction_Sub1_SayMyNumberEndpoint (ep_idrrs :: ExtraP) =
--    (:>) "SayMyNumber" ((:>) (ReqBody '[JSON] (NF1 "Sub1Action_SayMyNumber" "int" (MyInt ep_idrrs))) (Verb 'POST 200 ( '(:) JSON ('[] :: [Type])) [Char]))
-- extraParamAction_Sub1Server
--    :: forall (m :: Type -> Type) (ep_idrrs :: ExtraP)
--     . MonadUnliftIO m
--    => ActionRunner m (Sub1Action ep_idrrs)
--    -> ServerT (ExtraParamAction_Sub1Api ep_idrrs) m
-- extraParamAction_Sub1Server runner =
--    ( extraParamAction_Sub1_TalkToMeHandler runner
--        :<|> extraParamAction_Sub1_SayMyNumberHandler runner
--    )
-- extraParamAction_Sub1_TalkToMeHandler
--    :: forall (m :: Type -> Type) ep_idrrs
--     . MonadUnliftIO m
--    => ActionRunner m (Sub1Action ep_idrrs)
--    -> m Text
-- extraParamAction_Sub1_TalkToMeHandler runner_adryu =
--    runner_adryu TalkToMe
-- extraParamAction_Sub1_SayMyNumberHandler
--    :: forall (m :: Type -> Type) ep_idrrs
--     . MonadUnliftIO m
--    => ActionRunner m (Sub1Action ep_idrrs)
--    -> NF1 "Sub1Action_SayMyNumber" "int" (MyInt ep_idrrs)
--    -> m [Char]
-- extraParamAction_Sub1_SayMyNumberHandler
--    runner_adryw
--    (NF1 arg_adryv) =
--        runner_adryw (SayMyNumber arg_adryv)
-- type ExtraParamAction_Sub2Api =
--    ExtraParamAction_Sub2_SaySomethingEndpoint
-- type ExtraParamAction_Sub2_SaySomethingEndpoint =
--    (:>)
--        "SaySomething"
--        ( (:>)
--            ( QueryParam'
--                '[ Required
--                 , Strict
--                 ]
--                "number"
--                Int
--            )
--            (Verb 'GET 200 ( '(:) JSON ('[] :: [Type])) [Char])
--        )
-- extraParamAction_Sub2Server
--    :: forall (m :: Type -> Type)
--     . MonadUnliftIO m
--    => ActionRunner m Sub2Action
--    -> ServerT ExtraParamAction_Sub2Api m
-- extraParamAction_Sub2Server runner =
--    extraParamAction_Sub2_SaySomethingHandler runner
-- extraParamAction_Sub2_SaySomethingHandler
--    :: forall (m :: Type -> Type)
--     . MonadUnliftIO m
--    => ActionRunner m Sub2Action
--    -> Int
--    -> m [Char]
-- extraParamAction_Sub2_SaySomethingHandler runner_adryy arg_adryx =
--    runner_adryy (SaySomething arg_adryx)
-- type ExtraParamAction_Sub3Api a b =
--    ExtraParamAction_Sub3_SayHelloEndpoint
-- type ExtraParamAction_Sub3_SayHelloEndpoint =
--    (:>) "SayHello" (Verb 'GET 200 ( '(:) JSON ('[] :: [Type])) Text)
-- extraParamAction_Sub3Server
--    :: forall
--        (m :: Type -> Type)
--        (ep_idrrs :: ExtraP)
--        (bool_idrrr :: Bool)
--     . MonadUnliftIO m
--    => ActionRunner m (Sub3Action ep_idrrs bool_idrrr)
--    -> ServerT (ExtraParamAction_Sub3Api ep_idrrs bool_idrrr) m
-- extraParamAction_Sub3Server runner =
--    extraParamAction_Sub3_SayHelloHandler runner
-- extraParamAction_Sub3_SayHelloHandler
--    :: forall (m :: Type -> Type) ep_idrrs bool_idrrr
--     . MonadUnliftIO m
--    => ActionRunner m (Sub3Action ep_idrrs bool_idrrr)
--    -> m Text
-- extraParamAction_Sub3_SayHelloHandler runner_adryz =
--    runner_adryz SayHello
-- extraParamActionServer
--    :: forall
--        (m :: Type -> Type)
--        (bool_idrrr :: Bool)
--        (ep_idrrs :: ExtraP)
--     . MonadUnliftIO m
--    => ActionRunner m (ExtraParamAction bool_idrrr ep_idrrs)
--    -> ServerT (ExtraParamActionApi ep_idrrs bool_idrrr) m
-- extraParamActionServer runner =
--    ( extraParamAction_ReverseTextHandler runner
--        :<|> ( extraParamAction_Sub1Handler runner
--                :<|> ( extraParamAction_Sub2Handler runner
--                        :<|> extraParamAction_Sub3Handler runner
--                     )
--             )
--    )
-- extraParamAction_ReverseTextHandler
--    :: forall (m :: Type -> Type) bool_idrrr ep_idrrs
--     . MonadUnliftIO m
--    => ActionRunner m (ExtraParamAction bool_idrrr ep_idrrs)
--    -> NF1 "ExtraParamAction_ReverseText" "text" Text
--    -> m Text
-- extraParamAction_ReverseTextHandler runner_adryB (NF1 arg_adryA) =
--    runner_adryB (Action.ExtraParam.ReverseText arg_adryA)
-- extraParamAction_Sub1Handler
--    :: forall
--        (m :: Type -> Type)
--        (bool_idrrr :: Bool)
--        (ep_idrrs :: ExtraP)
--     . MonadUnliftIO m
--    => ActionRunner m (ExtraParamAction bool_idrrr ep_idrrs)
--    -> ServerT (ExtraParamAction_Sub1Api ep_idrrs) m
-- extraParamAction_Sub1Handler runner_adryC =
--    extraParamAction_Sub1Server (runner_adryC . Sub1)
-- extraParamAction_Sub2Handler
--    :: forall
--        (m :: Type -> Type)
--        (bool_idrrr :: Bool)
--        (ep_idrrs :: ExtraP)
--     . MonadUnliftIO m
--    => ActionRunner m (ExtraParamAction bool_idrrr ep_idrrs)
--    -> ServerT ExtraParamAction_Sub2Api m
-- extraParamAction_Sub2Handler runner_adryD =
--    extraParamAction_Sub2Server (runner_adryD . Sub2)
-- extraParamAction_Sub3Handler
--    :: forall
--        (m :: Type -> Type)
--        (bool_idrrr :: Bool)
--        (ep_idrrs :: ExtraP)
--     . MonadUnliftIO m
--    => ActionRunner m (ExtraParamAction bool_idrrr ep_idrrs)
--    -> ServerT (ExtraParamAction_Sub3Api ep_idrrs bool_idrrr) m
-- extraParamAction_Sub3Handler runner_adryE =
--    extraParamAction_Sub3Server (runner_adryE . Sub3)

-----------------------------------------------------------------------------------------
$(mkServer storeActionConfig ''StoreAction)

--

itemStuff
    :: ItemKey
    -> (NF1 "ItemAction_Buy" "quantity" Quantity -> ClientM NoContent)
        :<|> ClientM Quantity
        :<|> ClientM Price
listItems :: ClientM [ItemInfo]
search :: Text -> ClientM [ItemInfo]
adminOrder
    :: NF2 "AdminAction_Order" "item" ItemKey "quantity" Quantity -> ClientM NoContent
adminRestock
    :: NF2 "AdminAction_Restock" "itemKey" ItemKey "quantity" Quantity
    -> ClientM NoContent
adminAddItem
    :: NF3 "AdminAction_AddItem" "itemName" ItemName "quantity" Quantity "price" Price
    -> ClientM ItemKey
adminRemoveItem :: NF1 "AdminAction_RemoveItem" "item" ItemKey -> ClientM NoContent
listItems :<|> search :<|> itemStuff :<|> (adminOrder :<|> adminRestock :<|> adminAddItem :<|> adminRemoveItem) =
    client (Proxy @StoreActionApi)

--
--
type ExpectedReverseText =
    "ReverseText" :> ReqBody '[JSON] (NF1 "a" "text" Text) :> Post '[JSON] Text

type ExpectedIntersperse =
    "Sub"
        :> QueryParam' '[Strict, Required] "text" Text
        :> "Intersperse"
        :> QueryParam' '[Strict, Required] "intersperse_text" Text
        :> Get '[JSON] Text

$(mkServer testActionConfig ''ServerTestAction)

expectedReverseText :: NF1 "a" "text" Text -> ClientM Text
expectedReverseText = client (Proxy @ExpectedReverseText)

-- expectedConcatText :: Text -> Text -> ClientM Text
-- expectedConcatText = client (Proxy @ExpectedConcatText)

expectedIntersperseText :: Text -> Text -> ClientM Text
expectedIntersperseText = client (Proxy @ExpectedIntersperse)

-- writeOpenApi :: IO ()
-- writeOpenApi =
--    BL.writeFile "/tmp/store_schema.json" $ encode $ toOpenApi (Proxy @StoreActionApi)
--
withStoreServer :: IO () -> IO ()
withStoreServer runTests = do
    p <- createForgetful applyStoreEvent mempty
    server <-
        async . run 9898 $
            serve (Proxy @StoreActionApi) $
                hoistServer
                    (Proxy @StoreActionApi)
                    (Handler . ExceptT . try)
                    (storeActionServer $ runAction p handleStoreAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

withTestServer :: IO () -> IO ()
withTestServer runTests = do
    p <- createForgetful (\m _ -> m) ()
    -- server <- async . run 9898 $ serve (Proxy @StoreActionApi) undefined
    server <-
        async . run 9898 $
            serve (Proxy @ServerTestActionApi) $
                hoistServer
                    (Proxy @ServerTestActionApi)
                    (Handler . ExceptT . try)
                    (serverTestActionServer $ runAction p handleAction)
    threadDelay 10000 -- Ensure the server is live when the tests run
    runTests
    cancel server

spec :: Spec
spec = do
    clientEnv <- runIO $ do
        man <- newManager defaultManagerSettings
        pure $ mkClientEnv man (BaseUrl Http "localhost" 9898 "")
    aroundAll_ withStoreServer $ do
        describe "Server endpoint renaming" $ do
            it "Can add item" $ do
                r <- runClientM (adminAddItem $ NF3 "Test item" 10 99) clientEnv
                r `shouldSatisfy` not . null
            it "The new item shows up when listing items" $ do
                r <- runClientM listItems clientEnv
                case r of
                    Right [ItemInfo _ n _ _ _] -> n `shouldBe` "Test item"
                    a -> fail $ "That shouldn't happen! " <> show a
    aroundAll_ withTestServer $ do
        describe "Endpoints generated as expected" $ do
            it "Plaintext endpoint works" $ do
                r <- runClientM (expectedReverseText $ NF1 "Hej") clientEnv
                r `shouldBe` Right "jeH"
            it "Produces the expected parameters for subserver" $ do
                r <- runClientM (expectedIntersperseText "hello" "-=") clientEnv
                r `shouldBe` Right "h=-=e=-=l=-=l=-=o"
    describe "Supports extra parameters on Actions" $ do
        it "Can get apiOptions with one extra paramter" $ do
            M.lookup "Action.ExtraParam.ExtraParamAction" (allApiOptions extraParamConfig)
                `shouldSatisfy` not . null
