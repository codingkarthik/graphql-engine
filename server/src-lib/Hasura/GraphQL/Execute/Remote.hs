module Hasura.GraphQL.Execute.Remote
  ( buildExecStepRemote
  , validateRemoteResponse
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Data.HashSet                           as Set
import qualified Data.Aeson                             as J

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import           Hasura.GraphQL.Execute.Prepare
import           Hasura.RQL.Types                       hiding (isListType)
import           Hasura.SQL.Types
import Debug.Trace

collectVariables
  :: forall fragments var
   . (Foldable fragments, Hashable var, Eq var)
  => G.SelectionSet fragments var
  -> Set.HashSet var
collectVariables =
  Set.unions . fmap (foldMap Set.singleton)

buildExecStepRemote
  :: forall db
   . RemoteSchemaInfo
  -> G.OperationType
  -> [G.VariableDefinition]
  -> G.SelectionSet G.NoFragments G.Name
  -> Maybe GH.VariableValues
  -> ValidateRemoteFieldInfo
  -> G.GType
  -> ExecutionStep db
buildExecStepRemote remoteSchemaInfo tp varDefs selSet varValsM fieldValidationInfo gType =
  let requiredVars = collectVariables selSet
      restrictedDefs = filter (\varDef -> G._vdName varDef `Set.member` requiredVars) varDefs
      restrictedValsM = flip Map.intersection (Set.toMap requiredVars) <$> varValsM
      typedOpDef = G.TypedOperationDefinition tp Nothing restrictedDefs [] selSet
      remoteCall =
        RemoteCall remoteSchemaInfo typedOpDef restrictedValsM fieldValidationInfo gType

  in ExecStepRemote remoteCall

validateRemoteResponse
  :: (MonadError QErr m)
  => ValidateRemoteFieldInfo
  -> G.GType
  -> J.Object
  -> m ()
validateRemoteResponse validationInfo gType respObject = do
  traceM ("\n\nresp is " <> (show respObject) <> "\n\n")
  traceM ("\n\nvalidationInfo is " <> (show validationInfo) <> "\n\n")
  let isNullable = G.isNullable gType
      isListType = G.isListType gType
--  respObject <- getObj resp
  case validationInfo of
    VRFScalar name -> do
      let scalarVal = Map.lookup (G.unName name) respObject
      case scalarVal of
        Nothing ->
          bool (throw400 ConstraintViolation $ "non-nullable value " <> name <<> " missing in the response")
               (pure ())
               isNullable
        (Just J.Null) ->
          bool (throw400 ConstraintViolation $ "null value provided at non-nullable type " <>> name)
               (pure ())
               isNullable
        (Just (J.String _)) -> pure ()
        (Just (J.Number _)) -> pure ()
        _ -> throw400 ConstraintViolation $ "scalar output can only be number or string"
    VRFObject name validationInfos ->
      trace ("****** reaching here too " <> (show name) <> " gtype is " <> (show gType) <> " object " <> (show respObject)) $
      let objVal = Map.lookup (G.unName name) respObject
      in
      trace ("\n\n objVal is " <> (show objVal)) $
      case objVal of
        Nothing ->
          bool (throw400 ConstraintViolation $ "non-nullable value " <> name <<> " missing in the response")
               (pure ())
               isNullable
        (Just J.Null) ->
          bool (throw400 ConstraintViolation $ "null value provided at non-nullable type " <>> name)
               (pure ())
               isNullable
        (Just (J.Object obj)) -> do
          when isListType $ do
            throw400 ConstraintViolation $ "Expected a list, but recieved an object at field: " <>> name
          flip traverse_ validationInfos $ \ (validationInfo', gType') ->
            validateRemoteResponse validationInfo' gType' $ obj
        (Just (J.Array arr)) -> do
          traceM ("\n\narray is " <> (show arr) <> "\n\n")
          unless isListType $ do
            throw400 ConstraintViolation $ "Expected an object, but recieved an list at field: " <>> name
          case validationInfos of
            -- This really ought to be the case, or the validationInfos was built wrong
            [(vInfo, gType')] -> flip traverse_ arr $ \val ->
                                  validateRemoteResponse vInfo gType' =<< getObj val
            _ -> throw500 "unexpected happened" -- FIXME: proper error message
        _ -> throw400 ConstraintViolation $ "Only list or object can be valid response type at field: " <>> name
    VRFEnum name possibleEnumValues ->
      let enumVal = Map.lookup (G.unName name) respObject
      in
      case enumVal of
        Nothing ->
          bool (throw400 ConstraintViolation $ "non-nullable value " <> name <<> " missing in the response")
               (pure ())
               isNullable
        (Just J.Null) ->
          bool (throw400 ConstraintViolation $ "null value provided at non-nullable type " <>> name)
               (pure ())
               isNullable
        (Just (J.String respEnumVal)) ->
          bool (throw400 ConstraintViolation $ "unknown enum value " <> respEnumVal <> " recieved in the response")
               (pure ())
               (respEnumVal `elem` (fmap (G.unName . G.unEnumValue) possibleEnumValues))
        _ -> throw400 ConstraintViolation $ "Only string type can be valid response for enum types at field: " <>> name
    _ -> pure ()
  where
    getObj = \case
      J.Object obj -> pure obj
      _            -> throw400 Unexpected $ "expected object but got " -- FIXME: proper error message
