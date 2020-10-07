module Hasura.GraphQL.Schema.Remote
  ( buildRemoteParser
--  , remoteFieldFullSchema
  , inputValueDefinitionParser
  , lookupObject
  , lookupType
  , lookupScalar
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Language.GraphQL.Draft.Syntax       as G
import qualified Data.List.NonEmpty                  as NE
import           Data.Type.Equality
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import           Data.Foldable                       (sequenceA_)
import           Data.List (nub)

import           Hasura.GraphQL.Parser               as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import           Hasura.GraphQL.Schema.Common

type RemoteField = Field NoFragments G.Name

buildRemoteParser
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => IntrospectionResult
  -> RemoteSchemaInfo
  -> m ( [P.FieldParser n (RemoteSchemaInfo, RemoteField)]
       , Maybe [P.FieldParser n (RemoteSchemaInfo, RemoteField)]
       , Maybe [P.FieldParser n (RemoteSchemaInfo, RemoteField)])
buildRemoteParser (IntrospectionResult sdoc query_root mutation_root subscription_root) info = do
  queryT <- makeParsers query_root
  mutationT <- traverse makeParsers mutation_root
  subscriptionT <- traverse makeParsers subscription_root
  return (queryT, mutationT, subscriptionT)
  where
    makeFieldParser :: G.FieldDefinition -> m (P.FieldParser n (RemoteSchemaInfo, RemoteField))
    makeFieldParser fieldDef = do
      fldParser <- remoteField' sdoc fieldDef
      pure $ fldParser <&> (\fp -> (info, fp))
    makeParsers :: G.Name -> m [P.FieldParser n (RemoteSchemaInfo, RemoteField)]
    makeParsers rootName =
      case lookupType sdoc rootName of
        Just (G.TypeDefinitionObject o) ->
          traverse makeFieldParser $ _otdFieldsDefinition o
        _ -> throw400 Unexpected $ rootName <<> " has to be an object type"

-- | 'remoteFieldFullSchema' takes the 'SchemaIntrospection' and a 'G.Name' and will
--   return a 'SelectionSet' parser if the 'G.Name' is found and is a 'TypeDefinitionObject',
--   otherwise, an error will be thrown.
-- remoteFieldFullSchema
--   :: forall n m
--    . (MonadSchema n m, MonadError QErr m)
--   => SchemaIntrospection
--   -> G.Name
--   -> m (Parser 'Output n (G.SelectionSet NoFragments Variable))
-- remoteFieldFullSchema sdoc name =
--   P.memoizeOn 'remoteFieldFullSchema name do
--   fieldObjectType <-
--     case lookupType sdoc name of
--       Just (G.TypeDefinitionObject o) -> pure o
--       _ -> throw400 RemoteSchemaError $ "object with " <> name <<> " not found"
--   fieldParser <- remoteSchemaObject sdoc fieldObjectType
--   pure $ P.unsafeRawParser (P.pType fieldParser)

remoteField'
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.FieldDefinition
  -> m (FieldParser n RemoteField)
remoteField' schemaDoc (G.FieldDefinition description name argsDefinition gType _) =
  let
    addNullableList :: FieldParser n RemoteField -> FieldParser n RemoteField
    addNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (Nullable (TList typ)))) parser

    addNonNullableList :: FieldParser n RemoteField -> FieldParser n RemoteField
    addNonNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (NonNullable (TList typ)))) parser

    -- TODO add directives, deprecation
    convertType :: G.GType -> m (FieldParser n RemoteField)
    convertType gType' = do
        case gType' of
          G.TypeNamed (Nullability True) fieldTypeName ->
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability True) gType'' ->
            addNullableList <$> convertType gType''
          G.TypeNamed (Nullability False) fieldTypeName -> do
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability False) gType'' ->
            addNonNullableList <$> convertType gType''
  in convertType gType

-- | 'remoteSchemaObject' returns a output parser for a given 'ObjectTypeDefinition'.
remoteSchemaObject
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.ObjectTypeDefinition
  -> m (Parser 'Output n (SelectionSet NoFragments G.Name))
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) subFields
  interfaceDefs <- traverse getInterface interfaces
  implements <- traverse (remoteSchemaInterface schemaDoc) interfaceDefs
  -- TODO: also check sub-interfaces, when these are supported in a future graphql spec
  traverse_ validateImplementsFields interfaceDefs
  pure $ P.selectionSetObject name description subFieldParsers implements <&>
    toList . (OMap.mapWithKey $ \alias -> \case
        P.SelectField fld  -> G.SelectionField fld
        P.SelectTypename _ ->
          G.SelectionField (G.Field (Just alias) $$(G.litName "__typename") mempty mempty mempty)
    )
  where
    getInterface :: G.Name -> m (G.InterfaceTypeDefinition [G.Name])
    getInterface interfaceName =
      onNothing (lookupInterface schemaDoc interfaceName) $
        throw400 RemoteSchemaError $ "Could not find interface " <> squote interfaceName
        <> " implemented by Object type " <> squote name
    validateImplementsFields :: G.InterfaceTypeDefinition [G.Name] -> m ()
    validateImplementsFields interface =
      traverse_ (validateImplementsField (_itdName interface)) (G._itdFieldsDefinition interface)
    validateImplementsField :: G.Name -> G.FieldDefinition -> m ()
    validateImplementsField interfaceName interfaceField =
      case lookup (G._fldName interfaceField) (zip (fmap G._fldName subFields) subFields) of
        Nothing -> throw400 RemoteSchemaError $
          "Interface field " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
          <> " expected, but " <> squote name <> " does not provide it"
        Just f -> do
          unless (validateSubType (G._fldType f) (G._fldType interfaceField)) $
            throw400 RemoteSchemaError $
            "The type of Object field " <> squote name <> "." <> dquote (G._fldName f)
            <> " (" <> G.showGT (G._fldType f)
            <> ") is not the same type/sub type of Interface field "
            <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
            <> " (" <> G.showGT (G._fldType interfaceField) <> ")"
          traverse_ (validateArgument (G._fldArgumentsDefinition f)) (G._fldArgumentsDefinition interfaceField)
          traverse_ (validateNoExtraNonNull (G._fldArgumentsDefinition interfaceField)) (G._fldArgumentsDefinition f)
            where
              validateArgument :: G.ArgumentsDefinition -> G.InputValueDefinition -> m ()
              validateArgument objectFieldArgs ifaceArgument =
                case lookup (G._ivdName ifaceArgument) (zip (fmap G._ivdName objectFieldArgs) objectFieldArgs) of
                  Nothing ->
                    throw400 RemoteSchemaError $
                      "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                      <> "(" <> dquote (G._ivdName ifaceArgument) <> ":) required, but Object field " <> squote name <> "." <> dquote (G._fldName f)
                      <> " does not provide it"
                  Just a ->
                    unless (G._ivdType a == G._ivdType ifaceArgument) $
                      throw400 RemoteSchemaError $
                      "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                      <> "(" <> dquote (G._ivdName ifaceArgument) <> ":) expects type "
                      <> G.showGT (G._ivdType ifaceArgument)
                      <> ", but " <> squote name <> "." <> dquote (G._fldName f) <> "("
                      <> dquote (G._ivdName ifaceArgument) <> ":) has type "
                      <> G.showGT (G._ivdType a)
              validateNoExtraNonNull :: G.ArgumentsDefinition -> G.InputValueDefinition -> m ()
              validateNoExtraNonNull ifaceArguments objectFieldArg =
                case lookup (G._ivdName objectFieldArg) (zip (fmap G._ivdName ifaceArguments) ifaceArguments) of
                  Just _ -> pure ()
                  Nothing ->
                    unless (G.isNullable (G._ivdType objectFieldArg)) $
                    throw400 RemoteSchemaError $
                    "Object field argument " <>  squote name <> "." <> dquote (G._fldName f) <> "("
                      <> dquote (G._ivdName objectFieldArg) <> ":) is of required type "
                      <> G.showGT (G._ivdType objectFieldArg) <> ", but is not provided by Interface field "
                      <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
    validateSubType :: G.GType -> G.GType -> Bool
    -- TODO this ignores nullability which is probably wrong, even though the GraphQL spec is ambiguous
    validateSubType (G.TypeList _ x) (G.TypeList _ y) = validateSubType x y
    -- It is OK to "upgrade" the strictness
    validateSubType (G.TypeNamed (Nullability False) x) (G.TypeNamed (Nullability True) y) =
      validateSubType (G.TypeNamed (Nullability True) x) (G.TypeNamed (Nullability True) y)
    validateSubType (G.TypeNamed nx x) (G.TypeNamed ny y) =
      case (lookupType schemaDoc x , lookupType schemaDoc y) of
        (Just x' , Just y') -> nx == ny && validateSubTypeDefinition x' y'
        _ -> False
    validateSubType _ _ = False
    validateSubTypeDefinition x' y' | x' == y' = True
    validateSubTypeDefinition (TypeDefinitionObject otd) (TypeDefinitionInterface itd)
      = G._otdName otd `elem` G._itdPossibleTypes itd
    validateSubTypeDefinition (TypeDefinitionObject _otd) (TypeDefinitionUnion _utd)
      = True -- TODO write appropriate check (may require saving 'possibleTypes' in Syntax.hs)
    validateSubTypeDefinition _ _ = False

-- | 'remoteSchemaInterface' returns a output parser for a given 'InterfaceTypeDefinition'.
remoteSchemaInterface
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.InterfaceTypeDefinition [G.Name]
  -> m (Parser 'Output n (G.SelectionSet NoFragments G.Name))
remoteSchemaInterface schemaDoc defn@(G.InterfaceTypeDefinition description name _directives fields possibleTypes) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) fields
  objs :: [Parser 'Output n (Name, (SelectionSet NoFragments G.Name))] <-
    traverse (\objName -> do
                 obj <- getObject objName
                 rsObj <- remoteSchemaObject schemaDoc obj
                 return $ (objName,) <$> rsObj)
             possibleTypes
  -- In the Draft GraphQL spec (> June 2018), interfaces can themselves
  -- implement superinterfaces.  In the future, we may need to support this
  -- here.
  when (null subFieldParsers) $
    throw400 RemoteSchemaError $ "List of fields cannot be empty for interface " <> squote name
  -- TODO: another way to obtain 'possibleTypes' is to lookup all the object
  -- types in the schema document that claim to implement this interface.  We
  -- should have a check that expresses that that collection of objects is equal
  -- to 'possibleTypes'.
  pure $ P.selectionSetInterface name description subFieldParsers objs <&>
    (\selSets ->
       let getCommonFields selFld acc =
             case selFld of
               G.SelectionField (Field { _fName }) ->
                 filter (not . (== _fName)) acc
               -- TODO: verify this once!
               -- Since the selection field is already in a fragment
               -- we can return the field as it is.
               _ -> mempty
           commonFieldsFn = foldr getCommonFields (map G._fldName fields)
           commonFields = concat $ traverse (commonFieldsFn . snd) $ selSets

           filterCommonFields e =
             case e of
             -- TODO: verify this once!
               G.SelectionField (Field { _fName }) -> _fName `elem` commonFields
               _                                   -> False

           removeCommonFields = map (\(objName,selSet) ->
                                                (objName, filter (not . filterCommonFields) selSet)) selSets

           commonSelectFields = nub $ concat $ map (filter filterCommonFields . snd) selSets

           mkObjInlineFragment (objName, selSet) =
             G.SelectionInlineFragment $
               G.InlineFragment (Just objName) mempty selSet
       in commonSelectFields <> map mkObjInlineFragment removeCommonFields
    )
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Interface " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Interface type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.UnionTypeDefinition
  -> m (Parser 'Output n (SelectionSet NoFragments G.Name))
remoteSchemaUnion schemaDoc defn@(G.UnionTypeDefinition description name _directives objectNames) =
  P.memoizeOn 'remoteSchemaObject defn do
  objs :: [Parser 'Output n (Name, (SelectionSet NoFragments G.Name))] <-
    traverse (\objName -> do
                 obj <- getObject objName
                 rsObj <- remoteSchemaObject schemaDoc obj
                 return $ (objName,) <$> rsObj)
             objectNames
  when (null objs) $
    throw400 RemoteSchemaError $ "List of member types cannot be empty for union type " <> squote name
  pure $ P.selectionSetUnion name description objs <&>
    (\selSets ->
       flip map selSets $ \(objName, selSet) ->
         (G.SelectionInlineFragment $
            G.InlineFragment (Just objName) mempty selSet))
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Union " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Union type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

-- | remoteSchemaInputObject returns an input parser for a given 'G.InputObjectTypeDefinition'
remoteSchemaInputObject
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.InputObjectTypeDefinition
  -> m (Parser 'Input n ())
remoteSchemaInputObject schemaDoc defn@(G.InputObjectTypeDefinition desc name _ valueDefns) =
  P.memoizeOn 'remoteSchemaInputObject defn do
  argsParser <- argumentsParser valueDefns schemaDoc
  pure $ P.object name desc argsParser

lookupType :: SchemaIntrospection -> G.Name -> Maybe (G.TypeDefinition [G.Name])
lookupType (SchemaIntrospection types) name = find (\tp -> getNamedTyp tp == name) types
  where
    getNamedTyp :: G.TypeDefinition possibleTypes -> G.Name
    getNamedTyp ty = case ty of
      G.TypeDefinitionScalar t      -> G._stdName t
      G.TypeDefinitionObject t      -> G._otdName t
      G.TypeDefinitionInterface t   -> G._itdName t
      G.TypeDefinitionUnion t       -> G._utdName t
      G.TypeDefinitionEnum t        -> G._etdName t
      G.TypeDefinitionInputObject t -> G._iotdName t

lookupObject :: SchemaIntrospection -> G.Name -> Maybe G.ObjectTypeDefinition
lookupObject (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe G.ObjectTypeDefinition
    go ((G.TypeDefinitionObject t):tps)
      | G._otdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupInterface :: SchemaIntrospection -> G.Name -> Maybe (G.InterfaceTypeDefinition [G.Name])
lookupInterface (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe (G.InterfaceTypeDefinition possibleTypes)
    go ((G.TypeDefinitionInterface t):tps)
      | G._itdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupScalar :: SchemaIntrospection -> G.Name -> Maybe G.ScalarTypeDefinition
lookupScalar (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe G.ScalarTypeDefinition
    go ((G.TypeDefinitionScalar t):tps)
      | G._stdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

-- | 'remoteFieldFromName' accepts a GraphQL name and searches for its definition
--   in the 'SchemaIntrospection'.
remoteFieldFromName
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> Maybe G.Description
  -> G.Name
  -> G.ArgumentsDefinition
  -> m (FieldParser n RemoteField)
remoteFieldFromName sdoc fieldName description fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> fieldName
    Just typeDef -> remoteField sdoc fieldName description argsDefns typeDef

-- | 'inputValuefinitionParser' accepts a 'G.InputValueDefinition' and will return an
--   'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of
--    the 'InputValueDefinition' then an error will be thrown.
inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => G.SchemaIntrospection
  -> G.InputValueDefinition
  -> m (InputFieldsParser n (Maybe (InputValue Variable)))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (InputValue Variable))
      fieldConstructor parser =
        let wrappedParser :: Parser k n (InputValue Variable)
            wrappedParser =
              P.Parser
                { P.pType   = P.pType parser
                , P.pParser = \value -> P.pParser parser value $> castWith (P.inputParserInput @k) value
                }
        in case maybeDefaultVal of
          Nothing ->
            if G.isNullable fieldType
            then fieldOptional name desc wrappedParser
            else Just <$> field name desc wrappedParser
          Just defaultVal -> Just <$> fieldWithDefault name desc defaultVal wrappedParser
      doNullability :: forall k . 'Input <: k => G.Nullability -> Parser k n () -> Parser k n ()
      doNullability (G.Nullability True)  = void . P.nullable
      doNullability (G.Nullability False) = id
      buildField
        :: G.GType
        -> (forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (InputValue Variable)))
        -> m (InputFieldsParser n (Maybe (InputValue Variable)))
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed nullability typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> typeName
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar scalarTypeDefn ->
                 pure $ fieldConstructor' $ doNullability nullability $ remoteFieldScalarParser scalarTypeDefn
               G.TypeDefinitionEnum defn ->
                 pure $ fieldConstructor' $ doNullability nullability $ remoteFieldEnumParser defn
               G.TypeDefinitionObject _ -> throw400 RemoteSchemaError "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn ->
                 fieldConstructor' . doNullability nullability <$> remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> throw400 RemoteSchemaError "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throw400 RemoteSchemaError "expected input type, but got output type"
       G.TypeList nullability subType -> buildField subType (fieldConstructor' . doNullability nullability . void . P.list)
  in buildField fieldType fieldConstructor

argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition
  -> G.SchemaIntrospection
  -> m (InputFieldsParser n ())
argumentsParser args schemaDoc = do
  sequenceA_ <$> flip traverse args (inputValueDefinitionParser schemaDoc)

inputValueVariableToGValue :: InputValue Variable -> G.Value G.Name
inputValueVariableToGValue (JSONValue _j) = error "handle this better"
inputValueVariableToGValue (GraphQLValue v) = getName <$> v

argumentsParser1
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition
  -> G.SchemaIntrospection
  -> m (InputFieldsParser n (HashMap Name (G.Value G.Name)))
argumentsParser1 args schemaDoc =
  let parser =
        sequenceA <$> flip traverse args
          (\inpValDef -> do
              p <- inputValueDefinitionParser schemaDoc inpValDef
              pure $ p `mapField` ((G._ivdName inpValDef),))
  in (fmap . fmap) (fmap inputValueVariableToGValue . Map.fromList . catMaybes) parser

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> Maybe G.Description
  -> G.ArgumentsDefinition
  -> G.TypeDefinition [G.Name]
  -> m (FieldParser n RemoteField)
remoteField sdoc fieldName description argsDefn typeDefn = do
  -- TODO add directives
  argsParser1 <- argumentsParser1 argsDefn sdoc
  case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      remoteSchemaObj <- remoteSchemaObject sdoc objTypeDefn
      let objSelection = P.subselectionWithAlias fieldName description argsParser1 remoteSchemaObj
      pure $ objSelection <&> (\(alias, args, selSet) ->
                                 (G.Field alias fieldName args mempty selSet))
    G.TypeDefinitionScalar scalarTypeDefn ->
      let scalarField = remoteFieldScalarParser scalarTypeDefn
          scalarSelection = P.selectionWithAlias fieldName description argsParser1 scalarField
      in
      pure $ (scalarSelection <&> (\(alias, args) -> (G.Field alias fieldName args mempty [])))
    G.TypeDefinitionEnum enumTypeDefn ->
      let enumField = remoteFieldEnumParser enumTypeDefn
          enumSelection = P.selectionWithAlias fieldName description argsParser1 enumField
      in
      pure $ enumSelection <&> (\(alias, _) -> (G.Field alias fieldName mempty mempty []))
    G.TypeDefinitionInterface ifaceTypeDefn -> do
      remoteSchemaObj <- remoteSchemaInterface sdoc ifaceTypeDefn
      pure $ P.subselectionWithAlias fieldName description argsParser1 remoteSchemaObj <&>
        (\(alias, args, selSet) ->
           (G.Field alias fieldName args mempty selSet))
    G.TypeDefinitionUnion unionTypeDefn -> do
      remoteSchemaObj <- remoteSchemaUnion sdoc unionTypeDefn
      pure $ P.subselectionWithAlias fieldName description argsParser1 remoteSchemaObj <&>
        (\(alias, args, selSet) ->
           (G.Field alias fieldName args mempty selSet))
    _ -> throw400 RemoteSchemaError "expected output type, but got input type"

remoteFieldScalarParser
  :: MonadParse n
  => G.ScalarTypeDefinition
  -> Parser 'Both n ()
remoteFieldScalarParser (G.ScalarTypeDefinition description name _) =
  case G.unName name of
    "Boolean" -> P.boolean $> ()
    "Int" -> P.int $> ()
    "Float" -> P.float $> ()
    "String" -> P.string $> ()
    "ID" -> P.identifier $> ()
    _ -> P.unsafeRawScalar name description $> ()

remoteFieldEnumParser
  :: MonadParse n
  => G.EnumTypeDefinition
  -> Parser 'Both n ()
remoteFieldEnumParser (G.EnumTypeDefinition desc name _ valueDefns) =
  let enumValDefns = valueDefns <&> \(G.EnumValueDefinition enumDesc enumName _) ->
        (mkDefinition (G.unEnumValue enumName) enumDesc P.EnumValueInfo,())
  in P.enum name desc $ NE.fromList enumValDefns
