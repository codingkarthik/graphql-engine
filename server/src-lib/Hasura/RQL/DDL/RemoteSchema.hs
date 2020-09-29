{-# LANGUAGE ViewPatterns #-}
module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , addRemoteSchemaP2
  , runIntrospectRemoteSchema
  , addRemoteSchemaToCatalog
  , addRemoteSchemaPermissionsToCatalog
  , runAddRemoteSchemaPermissions
  ) where

import           Control.Monad.Unique
import           Hasura.EncJSON
-- import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.RemoteServer
-- import           Hasura.GraphQL.Schema.Merge
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.RemoteSchema.Validate

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as S
import qualified Database.PG.Query           as Q

import           Hasura.RQL.Types
import           Hasura.Server.Version             (HasVersion)
import           Hasura.SQL.Types

import qualified Data.Environment                  as Env

runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     )
  => Env.Environment
  -> AddRemoteSchemaQuery
  -> m EncJSON
runAddRemoteSchema env q = do
  addRemoteSchemaP1 name
  addRemoteSchemaP2 env q
  buildSchemaCacheFor $ MORemoteSchema name
  pure successMsg
  where
    name = _arsqName q

runAddRemoteSchemaPermissions
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     )
  => AddRemoteSchemaPermissions
  -> m EncJSON
runAddRemoteSchemaPermissions q = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  upstreamRemoteSchema <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists "no such remote schema"
  resolveRoleBasedRemoteSchema providedSchemaDoc $ irDoc $ rscIntro $ rscpContext upstreamRemoteSchema
  liftTx $ addRemoteSchemaPermissionsToCatalog q
--  buildSchemaCacheFor $ MORemoteSchema name
  pure successMsg
  where
    name = _arspRemoteSchema q

    providedSchemaDoc = _rspdSchema $ _arspDefinition q

addRemoteSchemaP1
  :: (QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup
  :: (HasVersion, QErrM m, MonadIO m, MonadUnique m, HasHttpManager m)
  => Env.Environment
  -> AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup env (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi

addRemoteSchemaP2
  :: (HasVersion, MonadTx m, MonadIO m, MonadUnique m, HasHttpManager m) => Env.Environment -> AddRemoteSchemaQuery -> m ()
addRemoteSchemaP2 env q = do
  void $ addRemoteSchemaP2Setup env q
  liftTx $ addRemoteSchemaToCatalog q

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  removeRemoteSchemaP1 rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
  when (depObjs /= []) $ reportDeps depObjs
  where
    remoteSchemaDepId = SORemoteSchema rsn

runReloadRemoteSchema
  :: (QErrM m, CacheRWM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  remoteSchemas <- getAllRemoteSchemas <$> askSchemaCache
  unless (name `elem` remoteSchemas) $ throw400 NotExists $
    "remote schema with name " <> name <<> " does not exist"

  let invalidations = mempty { ciRemoteSchemas = S.singleton name }
  withNewInconsistentObjsCheck $ buildSchemaCacheWithOptions CatalogUpdate invalidations
  pure successMsg

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ J.toJSON def, comment) True

addRemoteSchemaPermissionsToCatalog
  :: AddRemoteSchemaPermissions
  -> Q.TxE QErr ()
addRemoteSchemaPermissionsToCatalog (AddRemoteSchemaPermissions name role def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_remote_schema_permission
      (remote_schema_name, role_name, definition, comment)
      VALUES ($1, $2, $3, $4)
  |] (name, role, Q.AltJ $ J.toJSON def, comment) True

removeRemoteSchemaFromCatalog :: RemoteSchemaName -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True

fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     ORDER BY name ASC
     |] () True
  where
    fromRow (name, Q.AltJ def, comment) =
      AddRemoteSchemaQuery name def comment

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  (RemoteSchemaCtx _ _ _ introspectionByteString _) <-
    rscpContext <$> (onNothing (Map.lookup rsName (scRemoteSchemas sc)) $
    throw400 NotExists $
    "remote schema: " <> rsName <<> " not found")
  pure $ encJFromLBS introspectionByteString
