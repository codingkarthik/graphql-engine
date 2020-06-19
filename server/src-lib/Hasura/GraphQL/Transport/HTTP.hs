-- | Execution of GraphQL queries over HTTP transport
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  -- * imported from HTTP.Protocol; required by pro
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                 (MonadQueryLog (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

-- | Run (execute) a single GraphQL query
runGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     )
  => RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan pgExecCtx planCache
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.QueryExecutionPlan queryPlan -> do
        case queryPlan of
          E.ExecStepDB txGenSql -> do
            (telemTimeIO, telemQueryType, resp) <- runQueryDB reqId reqUnparsed userInfo txGenSql
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp []))
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $ do
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.MutationExecutionPlan mutationPlan -> do
        case mutationPlan of
          E.ExecStepDB (tx, responseHeaders) -> do
            (telemTimeIO, telemQueryType, resp) <- runMutationDB reqId reqUnparsed userInfo tx
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp responseHeaders))
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $ do
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.SubscriptionExecutionPlan _sub -> do
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
{-
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, respHdrs, resp) <- runHasuraGQ reqId reqUnparsed userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                           | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHeaders reqUnparsed rsi $ G._todType opDef
        return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))
-}
  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp
  where
    runRemoteGQ telemCacheHit rsi opDef = do
      let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                         | otherwise = Telem.Query
      (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHeaders reqUnparsed rsi opDef
      return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs')
runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     )
  => RequestId
  -> ResponseInternalErrorsConfig
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLBatchedReqs GQLQueryText
  -- ^ the batched request with unparsed GraphQL query
  -> m (HttpResponse EncJSON)
runGQBatched reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query = do
  case query of
    GQLSingleRequest req ->
      runGQ reqId userInfo ipAddress reqHdrs queryType req
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)

      removeHeaders <$> traverse (try . runGQ reqId userInfo ipAddress reqHdrs queryType) reqs
  where
    try = flip catchError (pure . Left) . fmap Right


runQueryDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> (LazyRespTx, EQ.GeneratedSqlMap)
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runQueryDB reqId query _userInfo (tx, genSql) = do
  -- log the generated SQL and the graphql query
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  logQueryLog logger query (Just genSql) reqId
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ do
    runQueryTx pgExecCtx tx
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Query
  return (telemTimeIO, telemQueryType, json)

runMutationDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> LazyRespTx
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutationDB reqId query userInfo tx = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  -- log the graphql query
  logQueryLog logger query Nothing reqId
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ do
    runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo tx
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Mutation
  return (telemTimeIO, telemQueryType, json)

{-
runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId query userInfo resolvedOp = do
  (E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _) <- ask
  logQuery' logger
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx _genSql -> do
      -- log the generated SQL and the graphql query
      -- L.unLogger logger $ QueryLog query genSql reqId
      ([],) <$> runQueryTx pgExecCtx tx
    E.ExOpMutation respHeaders tx -> do
      (respHeaders,) <$> runLazyTx pgExecCtx Q.ReadWrite (withUserInfo userInfo tx)
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"

  (respHdrs, resp) <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, json)
-}
