{-# LANGUAGE OverloadedRecordDot #-}

module CrmBeamProjection
    ( CrmDb
    , createCrmBackend
    )
where

import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Pool.Introspection (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Migrate
    ( CheckedDatabaseSettings
    , defaultMigratableDbSettings
    , renameCheckedEntity
    )
import Database.Beam.Postgres
import Database.PostgreSQL.Simple (execute_)
import DomainDriven
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Types (quoteIdent)
import Event
import EventHandler qualified
import Model (CrmModel (..))
import Types
import Prelude

data CustomerRowT f = CustomerRow
    { customerId :: Columnar f UUID
    , name :: Columnar f Text
    , email :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table CustomerRowT where
    data PrimaryKey CustomerRowT f = CustomerRowId (Columnar f UUID)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey row = CustomerRowId row.customerId

type CustomerRow = CustomerRowT Identity

data OrderRowT f = OrderRow
    { orderId :: Columnar f UUID
    , customerId :: Columnar f UUID
    , description :: Columnar f Text
    , status :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table OrderRowT where
    data PrimaryKey OrderRowT f = OrderRowId (Columnar f UUID)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey row = OrderRowId row.orderId

type OrderRow = OrderRowT Identity

data ItemRowT f = ItemRow
    { itemId :: Columnar f UUID
    , orderId :: Columnar f UUID
    , productName :: Columnar f Text
    , quantity :: Columnar f Int32
    , unitPrice :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table ItemRowT where
    data PrimaryKey ItemRowT f = ItemRowId (Columnar f UUID)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey row = ItemRowId row.itemId

type ItemRow = ItemRowT Identity

data CrmDb f = CrmDb
    { customersTable :: f (TableEntity CustomerRowT)
    , ordersTable :: f (TableEntity OrderRowT)
    , itemsTable :: f (TableEntity ItemRowT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

checkedCrmDb :: CheckedDatabaseSettings Postgres CrmDb
checkedCrmDb =
    defaultMigratableDbSettings
        `withDbModification` dbModification
            { customersTable = renameCheckedEntity (const "crm_customers")
            , ordersTable = renameCheckedEntity (const "crm_orders")
            , itemsTable = renameCheckedEntity (const "crm_order_items")
            }

createCrmBackend pool =
    postgresBeamBackend
        pool
        (InitialVersion "crm_events")
        BeamProjectionSpec
            { checkedDb = checkedCrmDb
            , dropProjection = dropCrmProjection
            , applyModelEvent = EventHandler.applyEvent
            , loadProjectionModel = \db _ -> loadCrmModelQuery db
            , projectStoredEvent = \db _ -> projectCrmEvent db
            }
createCrmBackend
    :: Pool Connection
    -> IO (PostgresBeam NoIndex CrmDb CrmModel CrmEvent)

dropCrmProjection :: Connection -> IO ()
dropCrmProjection conn = do
    void $ execute_ conn ("drop table if exists " <> quoteIdent "itemsTable")
    void $ execute_ conn ("drop table if exists " <> quoteIdent "ordersTable")
    void $ execute_ conn ("drop table if exists " <> quoteIdent "customersTable")

loadCrmModelQuery :: DatabaseSettings Postgres CrmDb -> Pg CrmModel
loadCrmModelQuery db = do
    customerRows <- runSelectReturningList $ select $ all_ (customersTable db)
    orderRows <- runSelectReturningList $ select $ all_ (ordersTable db)
    itemRows <- runSelectReturningList $ select $ all_ (itemsTable db)

    let itemsByOrder =
            Map.fromListWith (<>)
                [ let oid = OrderId row.orderId
                      iid = ItemId row.itemId
                  in  ( oid
                      , Map.singleton
                            iid
                            OrderItem
                                { itemId = iid
                                , productName = row.productName
                                , quantity = fromIntegral row.quantity
                                , unitPrice = fromIntegral row.unitPrice
                                }
                      )
                | row <- itemRows
                ]

        ordersByCustomer =
            Map.fromListWith (<>)
                [ let cid = CustomerId row.customerId
                      oid = OrderId row.orderId
                  in  ( cid
                      , Map.singleton
                            oid
                            Order
                                { orderId = oid
                                , description = row.description
                                , status = orderStatusFromText row.status
                                , items = Map.findWithDefault mempty oid itemsByOrder
                                }
                      )
                | row <- orderRows
                ]

        customers =
            Map.fromList
                [ let cid = CustomerId row.customerId
                  in  ( cid
                      , Customer
                            { customerId = cid
                            , name = row.name
                            , email = row.email
                            , orders = Map.findWithDefault mempty cid ordersByCustomer
                            }
                      )
                | row <- customerRows
                ]

    pure CrmModel{customers}

projectCrmEvent :: DatabaseSettings Postgres CrmDb -> Stored CrmEvent -> Pg ()
projectCrmEvent db (Stored event _timestamp _uuid) = case event of
    CustomerEvent{customerId = CustomerId cid, customerEvent} ->
        projectCustomerEvent db cid customerEvent

projectCustomerEvent :: DatabaseSettings Postgres CrmDb -> UUID -> CustomerEvent -> Pg ()
projectCustomerEvent db customerUuid event = case event of
    CustomerCreated{name, email} -> do
        deleteCustomerCascade db customerUuid
        runInsert $
            insert
                (customersTable db)
                (insertValues [CustomerRow{customerId = customerUuid, name, email}])
    CustomerRemoved ->
        deleteCustomerCascade db customerUuid
    CustomerNameChanged{name} -> do
        customerRow <- loadCustomerRow db customerUuid
        for_ customerRow $ \row ->
            runUpdate $
                save
                    (customersTable db)
                    CustomerRow
                        { customerId = row.customerId
                        , name = name
                        , email = row.email
                        }
    CustomerEmailChanged{email} -> do
        customerRow <- loadCustomerRow db customerUuid
        for_ customerRow $ \row ->
            runUpdate $
                save
                    (customersTable db)
                    CustomerRow
                        { customerId = row.customerId
                        , name = row.name
                        , email = email
                        }
    OrderEvent{orderId = OrderId orderUuid, orderEvent} ->
        projectOrderEvent db customerUuid orderUuid orderEvent

projectOrderEvent :: DatabaseSettings Postgres CrmDb -> UUID -> UUID -> OrderEvent -> Pg ()
projectOrderEvent db customerUuid orderUuid event = case event of
    OrderCreated{description} -> do
        customerRow <- loadCustomerRow db customerUuid
        for_ customerRow $ \_ -> do
            deleteOrderCascade db orderUuid
            runInsert $
                insert
                    (ordersTable db)
                    (insertValues [OrderRow{orderId = orderUuid, customerId = customerUuid, description, status = orderStatusToText Pending}])
    OrderStatusChanged{status} -> do
        orderRow <- loadOrderRow db customerUuid orderUuid
        for_ orderRow $ \row ->
            runUpdate $
                save
                    (ordersTable db)
                    OrderRow
                        { orderId = row.orderId
                        , customerId = row.customerId
                        , description = row.description
                        , status = orderStatusToText status
                        }
    OrderDescriptionChanged{description} -> do
        orderRow <- loadOrderRow db customerUuid orderUuid
        for_ orderRow $ \row ->
            runUpdate $
                save
                    (ordersTable db)
                    OrderRow
                        { orderId = row.orderId
                        , customerId = row.customerId
                        , description = description
                        , status = row.status
                        }
    OrderRemoved -> do
        orderExists <- isJust <$> loadOrderRow db customerUuid orderUuid
        when orderExists $
            deleteOrderCascade db orderUuid
    ItemAdded{itemId = ItemId itemUuid, productName, quantity, unitPrice} -> do
        orderRow <- loadOrderRow db customerUuid orderUuid
        for_ orderRow $ \_ -> do
            runDelete $
                delete
                    (itemsTable db)
                    (\row -> row.itemId ==. val_ itemUuid)
            runInsert $
                insert
                    (itemsTable db)
                    ( insertValues
                        [ ItemRow
                            { itemId = itemUuid
                            , orderId = orderUuid
                            , productName
                            , quantity = fromIntegral quantity
                            , unitPrice = fromIntegral unitPrice
                            }
                        ]
                    )
    ItemRemoved{itemId = ItemId itemUuid} -> do
        itemRow <- loadItemRow db orderUuid itemUuid
        for_ itemRow $ \_ ->
            runDelete $
                delete
                    (itemsTable db)
                    (\row -> row.itemId ==. val_ itemUuid)
    ItemQuantityChanged{itemId = ItemId itemUuid, quantity} -> do
        itemRow <- loadItemRow db orderUuid itemUuid
        for_ itemRow $ \row ->
            runUpdate $
                save
                    (itemsTable db)
                    ItemRow
                        { itemId = row.itemId
                        , orderId = row.orderId
                        , productName = row.productName
                        , quantity = fromIntegral quantity
                        , unitPrice = row.unitPrice
                        }

loadCustomerRow :: DatabaseSettings Postgres CrmDb -> UUID -> Pg (Maybe CustomerRow)
loadCustomerRow db customerUuid =
    runSelectReturningOne $
        select $ do
            row <- all_ (customersTable db)
            guard_ (row.customerId ==. val_ customerUuid)
            pure row

loadOrderRow :: DatabaseSettings Postgres CrmDb -> UUID -> UUID -> Pg (Maybe OrderRow)
loadOrderRow db customerUuid orderUuid =
    runSelectReturningOne $
        select $ do
            row <- all_ (ordersTable db)
            guard_ (row.customerId ==. val_ customerUuid)
            guard_ (row.orderId ==. val_ orderUuid)
            pure row

loadItemRow :: DatabaseSettings Postgres CrmDb -> UUID -> UUID -> Pg (Maybe ItemRow)
loadItemRow db orderUuid itemUuid =
    runSelectReturningOne $
        select $ do
            row <- all_ (itemsTable db)
            guard_ (row.orderId ==. val_ orderUuid)
            guard_ (row.itemId ==. val_ itemUuid)
            pure row

deleteCustomerCascade :: DatabaseSettings Postgres CrmDb -> UUID -> Pg ()
deleteCustomerCascade db customerUuid = do
    orderRows <-
        runSelectReturningList $
            select $ do
                row <- all_ (ordersTable db)
                guard_ (row.customerId ==. val_ customerUuid)
                pure row
    for_ orderRows $
        \row -> deleteOrderCascade db row.orderId
    runDelete $
        delete
            (customersTable db)
            (\row -> row.customerId ==. val_ customerUuid)

deleteOrderCascade :: DatabaseSettings Postgres CrmDb -> UUID -> Pg ()
deleteOrderCascade db orderUuid = do
    runDelete $
        delete
            (itemsTable db)
            (\row -> row.orderId ==. val_ orderUuid)
    runDelete $
        delete
            (ordersTable db)
            (\row -> row.orderId ==. val_ orderUuid)

orderStatusToText :: OrderStatus -> Text
orderStatusToText = \case
    Pending -> "pending"
    Confirmed -> "confirmed"
    Shipped -> "shipped"
    Cancelled -> "cancelled"

orderStatusFromText :: Text -> OrderStatus
orderStatusFromText = \case
    "pending" -> Pending
    "confirmed" -> Confirmed
    "shipped" -> Shipped
    "cancelled" -> Cancelled
    value -> error $ "Unexpected stored order status: " <> show value
