-- | Entry point for the code-generating executable `protoc` plugin. See the
-- | package README for instructions on how to run the code generator.
-- |
-- | The funny thing about writing a `protoc` compiler plugin codec is that it
-- | bootstraps itself. We just have to write enough of the compiler plugin codec
-- | that it can handle the `plugin.proto` and `descriptor.proto` files, and
-- | then we call the compiler plugin on these `.proto` files and the compiler
-- | plugin codec generates the rest of itself.
-- |
-- | Then we can delete the hand-written code and generate code to replace it
-- | with this command.
-- |
-- |     protoc --purescript_out=./src/ProtocPlugin google/protobuf/compiler/plugin.proto
-- |
-- | See
-- | * https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin.pb
-- | * https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.descriptor.pb
module ProtocPlugin.Main (main) where

import Prelude

import Data.Array (catMaybes, concatMap, filter, fold)
import Data.Array as Array
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView as DV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.String.Pattern as String.Pattern
import Data.String.Regex as String.Regex
import Data.String.Regex.Flags as String.Regex.Flags
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Google.Protobuf.Compiler.Plugin (CodeGeneratorRequest(..), CodeGeneratorResponse(..), CodeGeneratorResponse_File(..), parseCodeGeneratorRequest, putCodeGeneratorResponse)
import Google.Protobuf.Descriptor (DescriptorProto(..), EnumDescriptorProto(..), EnumValueDescriptorProto(..), FieldDescriptorProto(..), FieldDescriptorProto_Label(..), FieldDescriptorProto_Type(..), FieldOptions(..), FileDescriptorProto(..), OneofDescriptorProto(..))
import Node.Buffer (toArrayBuffer, fromArrayBuffer)
import Node.Encoding (Encoding(..))
import Node.Path (basenameWithoutExt)
import Node.Process (stdin, stdout, stderr)
import Node.Stream (read, write, writeString, onReadable)
import Text.Parsing.Parser (runParserT)


main :: Effect Unit
main = do
  onReadable stdin $ do
    stdinbufMay <- read stdin Nothing
    case stdinbufMay of
      Nothing -> pure unit
      Just stdinbuf -> do
        stdinab <- toArrayBuffer stdinbuf
        let stdinview = DV.whole stdinab
        requestParsed <- runParserT stdinview $ parseCodeGeneratorRequest $ DV.byteLength stdinview
        case requestParsed of
          Left err -> void $ writeString stderr UTF8 (show err) (pure unit)
          Right request -> do
            -- Uncomment this line to write the parsed declarations to stderr.
            -- void $ writeString stderr UTF8 (show request) (pure unit)
            let response = generate request
            responseab <- execPut $ putCodeGeneratorResponse response
            responsebuffer <- fromArrayBuffer responseab
            void $ write stdout responsebuffer (pure unit)


generate :: CodeGeneratorRequest -> CodeGeneratorResponse
generate (CodeGeneratorRequest{file_to_generate,parameter,proto_file,compiler_version}) = do
  case traverse (genFile proto_file) proto_file of
    Right file ->
      CodeGeneratorResponse
        { error: Nothing
        , file: file
        , supported_features: Nothing
        , __unknown_fields: []
        }
    Left err ->
      CodeGeneratorResponse
        { error: Just err
        , file: []
        , supported_features: Nothing
        , __unknown_fields: []
        }

 -- | Names of parent messages for a message or enum.
type NameSpace = Array String

-- | A message descriptor, plus the names of all parent messages.
data ScopedMsg = ScopedMsg NameSpace DescriptorProto

-- | An enum descriptor, plus the names of all parent messages.
data ScopedEnum = ScopedEnum NameSpace EnumDescriptorProto

-- | Scoped field name which has the qualified package namespace and the field name.
data ScopedField = ScopedField NameSpace String

-- | This is how we'll return errors while trying to generate the response from the request.
type Resp a = Either String a

genFile :: Array FileDescriptorProto -> FileDescriptorProto -> Resp CodeGeneratorResponse_File
genFile proto_file (FileDescriptorProto
  { name: fileName
  , package
  , dependency
  , public_dependency
  , message_type
  , enum_type
  , syntax
  }) = do

  let baseName = case fileName of
        Nothing -> "Generated"
        Just "" -> "Generated"
        Just n -> basenameWithoutExt n ".proto"

  messages :: Array ScopedMsg <- sequence $ flattenMessages [] message_type

  enums :: Array ScopedEnum <- Right (ScopedEnum [] <$> enum_type) <> sequence (flattenEnums [] message_type)

  let packageName = case package of -- Optional package, https://developers.google.com/protocol-buffers/docs/proto3#packages
        Nothing -> []
        Just ps -> String.split (String.Pattern.Pattern ".") ps

  let fileNameOut = baseName <> "." <> (String.joinWith "." ((map capitalize packageName))) <> ".purs"


  -- We have to import the modules qualified in the way because
  -- 1. When protoc "fully qualifies" a field type from an imported
  --    desriptor, the qualification consists of only the package name
  -- 2. protoc allows multiple files to have the same package name,
  --    such as descriptor.proto and any.proto (package "google.protobuf")
  --    but Purescript requires each file to have a different module name.
  let genImport :: String -> Resp String
      genImport fpath = do
        pkg <- lookupPackageByFilepath
        let moduleName = mkImportName fpath pkg
        let qualifiedName = Array.dropEnd 1 moduleName
        Right $ "import " <> make moduleName <> " as " <> make qualifiedName
       where
        make = String.joinWith "." <<< map capitalize
        lookupPackageByFilepath :: Resp (Array String)
        lookupPackageByFilepath =
          case Array.find (\(FileDescriptorProto f) -> maybe false (_ == fpath) f.name) proto_file of
            Just (FileDescriptorProto {package: Just p}) -> Right $ String.split (String.Pattern.Pattern ".") p
            _ -> Left $ "Failed lookupPackageByFilepath " <> fpath
        mkImportName
          :: String -- file path
          -> Array String -- package name
          -> Array String
        mkImportName fileString packages = map mkModuleName $ packages <> file
         where
          file = [basenameWithoutExt fileString ".proto"]


  let mkFieldType
        :: String -- prefix for the name, i.e. "put" "parse"
        -> String -- package-qualified period-separated field name
        -> String
      mkFieldType prefix s =
        let (ScopedField names name) = parseFieldName s
        in
        if names `beginsWith` packageName && (isLocalMessageName name || isLocalEnumName name)
          then
            -- it's a name in this package
            prefix <> (mkTypeName $ Array.drop (Array.length packageName) names <> [name])
          else
            -- it's a name in the top-level of an imported package
            String.joinWith "." $ (map mkModuleName $ names) <> [prefix <> capitalize name]
       where
        isLocalMessageName :: String -> Boolean
        isLocalMessageName fname = maybe false (const true) $
          flip Array.find messages $ \(ScopedMsg _ (DescriptorProto {name})) ->
            maybe false (fname == _) name
        isLocalEnumName :: String -> Boolean
        isLocalEnumName ename = maybe false (const true) $
          flip Array.find enums $ \(ScopedEnum _(EnumDescriptorProto {name})) ->
            maybe false (ename == _) name
        parseFieldName :: String -> ScopedField
        parseFieldName fname =
          if String.take 1 fname == "."
            then
              -- fully qualified
              let names = Array.dropWhile (_ == "") $ String.split (String.Pattern.Pattern ".") fname
              in
              ScopedField (Array.dropEnd 1 names) (fromMaybe "" $ Array.last names)
            else
              ScopedField [] fname -- this case should never occur, protoc always qualifies the names for us
        beginsWith :: Array String -> Array String -> Boolean
        beginsWith xs x = x == Array.take (Array.length x) xs

  -- We have an r and we're merging an l.
  -- About merging: https://github.com/protocolbuffers/protobuf/blob/master/docs/field_presence.md
  let genFieldMerge :: FieldDescriptorProto -> Resp (Maybe String)
      genFieldMerge (FieldDescriptorProto
        { oneof_index: Just _
        }) = Right $ Nothing -- Oneof case handled separately
      genFieldMerge (FieldDescriptorProto
        { name: Just name'
        , label: Just FieldDescriptorProto_Label_LABEL_REPEATED
        }) = Right $ Just $ fname <> ": r." <> fname <> " <> l." <> fname
       where fname = decapitalize name'
      genFieldMerge (FieldDescriptorProto
        { name: Just name'
        , label: Just _
        , type: Just FieldDescriptorProto_Type_TYPE_MESSAGE
        , type_name: Just tname
        }) = Right $ Just $ fname <> ": Runtime.mergeWith " <> mkFieldType "merge" tname <> " l." <> fname <> " r." <> fname
       where fname = decapitalize name'
      genFieldMerge (FieldDescriptorProto
        { name: Just name'
        , label: Just _
        , type: Just _
        }) = Right $ Just $ fname <> ": Alt.alt l." <> fname <> " r." <> fname
       where fname = decapitalize name'
      genFieldMerge _ = Left "Failed genFieldDefault missing FieldDescriptorProto name or label"

  let genFieldMergeOneof :: NameSpace -> Array FieldDescriptorProto -> Int -> OneofDescriptorProto -> Resp String
      genFieldMergeOneof nameSpace allfields oneof_index (OneofDescriptorProto {name: Just oname}) =
        Right $ fname <> ": merge" <> cname <> " l." <> fname <> " r." <> fname

       where
        fname = decapitalize oname
        cname = String.joinWith "_" $ map capitalize $ nameSpace <> [oname]
      genFieldMergeOneof _ _ _ _ = Left "Failed genFieldMergeOneof missing name"

  let genOneofMerge
        :: NameSpace
        -> Array FieldDescriptorProto
        -> Int
        -> OneofDescriptorProto
        -> Resp String
      genOneofMerge nameSpace allfields oneof_index (OneofDescriptorProto {name: Just oname}) = do
        Right $ "merge" <> cname <> " :: Maybe.Maybe " <> cname <> " -> Maybe.Maybe " <> cname <> " -> Maybe.Maybe " <> cname <> "\n"
          <> "merge" <> cname <> " l r = case Tuple.Tuple l r of\n"
          <> (fold $ catMaybes $ map genField fields)
          <> "  _ -> Alt.alt l r\n"
       where
        fields = filter ownfield allfields
        ownfield (FieldDescriptorProto {oneof_index: Just i}) = i == oneof_index
        ownfield _ = false
        cname = String.joinWith "_" $ map capitalize $ nameSpace <> [oname]
        genField :: FieldDescriptorProto -> Maybe String
        genField (FieldDescriptorProto
          { type: Just FieldDescriptorProto_Type_TYPE_MESSAGE
          , name: Just name_inner
          , type_name: Just tname
          }) = Just $ "  Tuple.Tuple (Maybe.Just (" <> fname_inner <> " l')) (Maybe.Just (" <> fname_inner <> " r')) -> map " <> fname_inner <> " $ Runtime.mergeWith " <> mkFieldType "merge" tname <> " (Maybe.Just l') (Maybe.Just r')\n"
         where
          fname_inner = String.joinWith "_" $ map capitalize [cname,name_inner]
        genField _ = Nothing
      genOneofMerge _ _ _ _ = Left "Failed genOneofMerge missing name"


  let genTypeOneof
        :: NameSpace
        -> Array FieldDescriptorProto
        -> Int
        -> OneofDescriptorProto
        -> Resp String
      genTypeOneof nameSpace pfields indexOneof (OneofDescriptorProto {name: Just oname}) = do
        fields <- catMaybes <$> traverse go pfields
        Right $ String.joinWith "\n"
          [ "data " <> cname
          , "  = " <> String.joinWith "\n  | " fields
          , ""
          , "derive instance generic" <> cname <> " :: Generic.Rep.Generic " <> cname <> " _"
          , "derive instance eq" <> cname <> " :: Eq.Eq " <> cname
          , "instance show" <> cname <> " :: Show.Show " <> cname <> " where show = Generic.Rep.Show.genericShow"
          , ""
          ]
       where
        cname = String.joinWith "_" $ map capitalize $ nameSpace <> [oname]
        go :: FieldDescriptorProto -> Resp (Maybe String)
        go (FieldDescriptorProto {name: Just fname, oneof_index: Just index, type: Just ftype, type_name}) = do
            if index == indexOneof
              then do
                fieldType <- genFieldType ftype type_name
                Right $ Just $ (String.joinWith "_" $ map capitalize [cname,fname]) <> " " <> fieldType
              else Right Nothing -- skip this Oneof
           where
            genFieldType :: FieldDescriptorProto_Type -> Maybe String -> Resp String
            genFieldType FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right "Number"
            genFieldType FieldDescriptorProto_Type_TYPE_FLOAT _ = Right "Float32.Float32"
            genFieldType FieldDescriptorProto_Type_TYPE_INT64 _ = Right "(Long.Long Long.Signed)"
            genFieldType FieldDescriptorProto_Type_TYPE_UINT64 _ = Right "(Long.Long Long.Unsigned)"
            genFieldType FieldDescriptorProto_Type_TYPE_INT32 _ = Right "Int"
            genFieldType FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right "(Long.Long Long.Unsigned)"
            genFieldType FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right "UInt.UInt"
            genFieldType FieldDescriptorProto_Type_TYPE_BOOL _ = Right "Boolean"
            genFieldType FieldDescriptorProto_Type_TYPE_STRING _ = Right "String"
            genFieldType FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ mkFieldType "" tname
            genFieldType FieldDescriptorProto_Type_TYPE_MESSAGE _ = Left "Failed genTypeOneof missing FieldDescriptorProto type_name"
            genFieldType FieldDescriptorProto_Type_TYPE_BYTES _ = Right "Common.Bytes"
            genFieldType FieldDescriptorProto_Type_TYPE_UINT32 _ = Right "UInt.UInt"
            genFieldType FieldDescriptorProto_Type_TYPE_ENUM (Just tname) = Right $ mkFieldType "" tname
            genFieldType FieldDescriptorProto_Type_TYPE_ENUM _ = Left "Failed genTypeOneof missing FieldDescriptorProto type_name"
            genFieldType FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right "Int"
            genFieldType FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right "(Long.Long Long.Signed)"
            genFieldType FieldDescriptorProto_Type_TYPE_SINT32 _ = Right "Int"
            genFieldType FieldDescriptorProto_Type_TYPE_SINT64 _ = Right "(Long.Long Long.Signed)"
            genFieldType FieldDescriptorProto_Type_TYPE_GROUP _ = Left "Failed genTypeOneof GROUP not supported."
        go _ = Right Nothing
      genTypeOneof _ _ _ arg = Left $ "Failed genTypeOneof missing OneofDescriptorProto name\n" <> show arg

  let genIsDefaultOneof
        :: NameSpace
        -> Array FieldDescriptorProto
        -> Int
        -> OneofDescriptorProto
        -> Resp String
      genIsDefaultOneof nameSpace pfields indexOneof (OneofDescriptorProto {name: Just oname}) = do
        fields <- catMaybes <$> traverse go pfields
        Right $ String.joinWith "\n"
          [ "isDefault" <> cname <> " :: " <> cname <> " -> Boolean"
          , String.joinWith "\n" fields
          , ""
          ]
       where
        cname = String.joinWith "_" $ map capitalize $ nameSpace <> [oname]
        go :: FieldDescriptorProto -> Resp (Maybe String)
        go (FieldDescriptorProto {name: Just fname, oneof_index: Just index, type: Just FieldDescriptorProto_Type_TYPE_MESSAGE, type_name}) =
          Right $ Just $ "isDefault" <> cname <> " (" <> (String.joinWith "_" $ map capitalize [cname,fname]) <> " _) = false"
        go (FieldDescriptorProto {name: Just fname, oneof_index: Just index, type: _, type_name}) =
          Right $ Just $ "isDefault" <> cname <> " (" <> (String.joinWith "_" $ map capitalize [cname,fname]) <> " x) = Common.isDefault x"
        go _ = Right Nothing
      genIsDefaultOneof _ _ _ arg = Left $ "Failed genIsDefaultOneof missing OneofDescriptorProto name\n" <> show arg


  let genOneofPut :: NameSpace -> Array FieldDescriptorProto -> Int -> OneofDescriptorProto -> Resp String
      genOneofPut nameSpace field oindex (OneofDescriptorProto {name: Just oname}) =
        map (String.joinWith "\n") $ sequence $
        [ Right $ "  case r." <> decapitalize oname <> " of"
        , Right "    Maybe.Nothing -> pure unit"
        ]
        <>
        (map genOneofFieldPut myfields)
       where
        myfields = Array.filter ismine field
        ismine f@(FieldDescriptorProto {oneof_index: Just i}) = i == oindex
        ismine _ = false
        genOneofFieldPut :: FieldDescriptorProto -> Resp String
        genOneofFieldPut (FieldDescriptorProto
          { name: Just name'
          , number: Just fnumber
          , type: Just ftype
          , type_name
          }) = go ftype type_name
         where
          fname = decapitalize name'
          -- If you set a oneof field to the default value (such as setting an int32 oneof field to 0), the "case" of that oneof field will be set, and the value will be serialized on the wire.
          -- https://developers.google.com/protocol-buffers/docs/proto3#oneof_features
          go FieldDescriptorProto_Type_TYPE_DOUBLE _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.double"
          go FieldDescriptorProto_Type_TYPE_FLOAT _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.float"
          go FieldDescriptorProto_Type_TYPE_INT64 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.int64"
          go FieldDescriptorProto_Type_TYPE_UINT64 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.uint64"
          go FieldDescriptorProto_Type_TYPE_INT32 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.int32"
          go FieldDescriptorProto_Type_TYPE_FIXED64 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.fixed64"
          go FieldDescriptorProto_Type_TYPE_FIXED32 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.fixed32"
          go FieldDescriptorProto_Type_TYPE_BOOL _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.bool"
          go FieldDescriptorProto_Type_TYPE_STRING _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.string"
          go FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) $ Runtime.putLenDel " <> mkFieldType "put" tname
          go FieldDescriptorProto_Type_TYPE_MESSAGE _ = Left "Failed genOneofPut missing FieldDescriptorProto type_name"
          go FieldDescriptorProto_Type_TYPE_BYTES _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.bytes"
          go FieldDescriptorProto_Type_TYPE_UINT32 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.uint32"
          go FieldDescriptorProto_Type_TYPE_ENUM _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Runtime.putEnum"
          go FieldDescriptorProto_Type_TYPE_SFIXED32 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.sfixed32"
          go FieldDescriptorProto_Type_TYPE_SFIXED64 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.sfixed64"
          go FieldDescriptorProto_Type_TYPE_SINT32 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.sint32"
          go FieldDescriptorProto_Type_TYPE_SINT64 _ =
            Right $ "    Maybe.Just (" <> mkTypeName (nameSpace <> [oname,name']) <> " x) -> Runtime.putOptional " <> show fnumber <> " (Maybe.Just x) (\\_ -> false) Encode.sint64"
          go FieldDescriptorProto_Type_TYPE_GROUP _ = Left "Failed genOneofPut GROUP not supported."
        genOneofFieldPut _ = Left "Failed genOneofPut missing FieldDescriptorProto name or number or type"
      genOneofPut _ _ _ _ = Left "Failed genOneofPut missing OneofDescriptoroProto name"



  let genFieldPut :: NameSpace -> FieldDescriptorProto -> Resp (Maybe String)
      genFieldPut nameSpace (FieldDescriptorProto
        { name: Just name'
        , number: Just fnumber
        , label: Just flabel
        , type: Just ftype
        , type_name
        , oneof_index: Nothing -- must not be a member of a oneof, that case handled seperately
        , options
        }) = go flabel ftype type_name options
       where
        fname = decapitalize name'
        -- For repeated fields of primitive numeric types, always put the packed
        -- encoding.
        -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_DOUBLE _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $ "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.double"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_DOUBLE _ _ =
          Right $ Just $ "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.double'"
        go _ FieldDescriptorProto_Type_TYPE_DOUBLE _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.double"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FLOAT _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.float"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FLOAT _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.float'"
        go _ FieldDescriptorProto_Type_TYPE_FLOAT _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.float"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT64 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.int64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT64 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.int64'"
        go _ FieldDescriptorProto_Type_TYPE_INT64 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.int64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT64 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.uint64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT64 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.uint64'"
        go _ FieldDescriptorProto_Type_TYPE_UINT64 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.uint64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT32 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.int32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT32 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.int32'"
        go _ FieldDescriptorProto_Type_TYPE_INT32 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.int32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED64 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.fixed64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED64 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.fixed64'"
        go _ FieldDescriptorProto_Type_TYPE_FIXED64 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.fixed64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED32 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.fixed32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED32 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.fixed32'"
        go _ FieldDescriptorProto_Type_TYPE_FIXED32 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.fixed32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BOOL _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.bool"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BOOL _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.bool'"
        go _ FieldDescriptorProto_Type_TYPE_BOOL _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.bool"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_STRING _ _ =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.string"
        go _ FieldDescriptorProto_Type_TYPE_STRING _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.string"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) _ =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " $ Runtime.putLenDel " <> mkFieldType "put" tname
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_MESSAGE _ _ =
          Left "Failed genFieldPut missing FieldDescriptorProto type_name"
        go _ FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " (\\_ -> false) $ Runtime.putLenDel " <> mkFieldType "put" tname
        go _ FieldDescriptorProto_Type_TYPE_MESSAGE _ _ =
          Left "Failed genFieldPut missing FieldDescriptorProto type_name"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BYTES _ _ =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " $ Encode.bytes"
        go _ FieldDescriptorProto_Type_TYPE_BYTES _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.bytes"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT32 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.uint32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT32 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.uint32'"
        go _ FieldDescriptorProto_Type_TYPE_UINT32 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.uint32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_ENUM _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Runtime.putEnum"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_ENUM _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Runtime.putEnum'"
        go _ FieldDescriptorProto_Type_TYPE_ENUM _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Runtime.putEnum"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED32 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.sfixed32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED32 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sfixed32'"
        go _ FieldDescriptorProto_Type_TYPE_SFIXED32 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.sfixed32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED64 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.sfixed64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED64 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sfixed64'"
        go _ FieldDescriptorProto_Type_TYPE_SFIXED64 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.sfixed64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT32 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.sint32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT32 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sint32'"
        go _ FieldDescriptorProto_Type_TYPE_SINT32 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.sint32"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT64 _ (Just (FieldOptions {packed:Just false})) =
          Right $ Just $  "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.sint64"
        go FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT64 _ _ =
          Right $ Just $  "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sint64'"
        go _ FieldDescriptorProto_Type_TYPE_SINT64 _ _ =
          Right $ Just $  "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Common.isDefault Encode.sint64"
        go _ FieldDescriptorProto_Type_TYPE_GROUP _ _ = Left "Failed genFieldPut GROUP not supported"
      genFieldPut _ (FieldDescriptorProto { oneof_index: Just _ }) = Right Nothing -- It's a Oneof, this case is handled separately
      genFieldPut _ arg = Left $ "Failed genFieldPut missing FieldDescriptorProto name or number or label or type\n" <> show arg

  let genFieldParser :: NameSpace -> Array OneofDescriptorProto -> FieldDescriptorProto -> Resp String
      genFieldParser nameSpace oneof_decl (FieldDescriptorProto
        { name: Just name'
        , number: Just fnumber
        , label: Just flabel
        , type: Just ftype
        , type_name
        , oneof_index
        }) = go (lookupOneof oneof_index) flabel ftype type_name
       where
        lookupOneof :: Maybe Int -> Maybe String
        lookupOneof Nothing = Nothing
        lookupOneof (Just i) =
          case Array.index oneof_decl i of
            Just (OneofDescriptorProto {name}) -> name
            _-> Nothing

        fname = decapitalize name'
        mkConstructor oname = mkTypeName (nameSpace <> [oname,name'])

        -- For repeated fields of primitive numeric types, also parse the packed
        -- encoding.
        -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.double"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.double"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FLOAT _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.float"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.float"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.int64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.uint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.int32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.fixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.fixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BOOL _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bool"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.bool"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_STRING _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.string"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel " <> mkFieldType "parse" tname
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BYTES _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bytes"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.uint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_ENUM _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseEnum"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Runtime.parseEnum"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.sfixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.sfixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.sint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]
        go _ FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Array.snoc x"
          , "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel $ Runtime.manyLength Decode.sint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Function.flip Semigroup.append x"
          ]

        go (Just oname) _ FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.double"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_FLOAT _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.float"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_INT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_UINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _  FieldDescriptorProto_Type_TYPE_INT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ ->  Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_BOOL _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bool"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_STRING _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.string"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel " <> mkFieldType "parse" tname
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ " <> mkFieldType "merge" cname <> " (Maybe.Just (" <> mkConstructor oname <> " x))"
          ]
            where cname = String.joinWith "_" $ map capitalize $ nameSpace <> [oname]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_BYTES _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bytes"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_UINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_ENUM _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseEnum"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_SINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]
        go (Just oname) _ FieldDescriptorProto_Type_TYPE_SINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> decapitalize oname <> "\") $ \\_ -> Maybe.Just (" <> mkConstructor oname <> " x)"
          ]

        go _ _ FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.double"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_FLOAT _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.float"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_INT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_UINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _  FieldDescriptorProto_Type_TYPE_INT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.int32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.fixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_BOOL _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bool"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_STRING _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.string"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ String.joinWith "\n"
            -- “merge all input elements if it's a message type field”
            -- https://developers.google.com/protocol-buffers/docs/proto3#updating
            -- https://developers.google.com/protocol-buffers/docs/encoding#optional
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseLenDel " <> mkFieldType "parse" tname
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ Maybe.Just <<< Maybe.maybe x (" <> mkFieldType "merge" tname <> " x)"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_MESSAGE _ = Left "Failed genFieldParser missing FieldDescriptorProto type_name"
        go _ _ FieldDescriptorProto_Type_TYPE_BYTES _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.LenDel = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.bytes"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_UINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.uint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_ENUM _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Runtime.parseEnum"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits32 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.Bits64 = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sfixed64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_SINT32 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint32"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_SINT64 _ = Right $ String.joinWith "\n"
          [ "  parseField " <> show fnumber <> " Common.VarInt = Runtime.label \"" <> name' <> " / \" $ do"
          , "    x <- Decode.sint64"
          , "    pure $ Record.Builder.modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ \\_ -> Maybe.Just x"
          ]
        go _ _ FieldDescriptorProto_Type_TYPE_GROUP _ = Left "Failed genFieldParser GROUP not supported"
      genFieldParser _ _ _ = Left "Failed genFieldParser missing FieldDescriptorProto name or number or label or type"

  -- | For embedded message fields, the parser merges multiple instances of the same field,
  -- | https://developers.google.com/protocol-buffers/docs/encoding?hl=en#optional
  let genFieldRecord :: NameSpace -> FieldDescriptorProto -> Resp (Maybe String)
      genFieldRecord nameSpace (FieldDescriptorProto
        { name: Just name'
        , number: Just fnumber
        , label: Just flabel
        , type: Just ftype
        , type_name
        , oneof_index
        }) = (map<<<map) (\x -> fname <> " :: " <> x) $ ptype oneof_index flabel ftype type_name
       where
        fname = decapitalize name'
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right $ Just "Array Number"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FLOAT _ = Right $ Just "Array Float32.Float32"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT64 _ = Right $ Just "Array (Long.Long Long.Signed)"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT64 _ = Right $ Just "Array (Long.Long Long.Unsigned)"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_INT32 _ = Right $ Just "Array Int"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right $ Just "Array (Long.Long Long.Unsigned)"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right $ Just "Array UInt.UInt"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BOOL _ = Right $ Just "Array Boolean"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_STRING _ = Right $ Just "Array String"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ Just $ "Array " <> mkFieldType "" tname
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_MESSAGE _ = Left "Failed genFieldRecord missing FieldDescriptorProto type_name"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_BYTES _ = Right $ Just "Array Common.Bytes"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_UINT32 _ = Right $ Just "Array UInt.UInt"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_ENUM (Just tname) = Right $ Just $ "Array " <> mkFieldType "" tname
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_ENUM _ = Left "Failed genFieldRecord missing FieldDescriptorProto type_name"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right $ Just "Array Int"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right $ Just "Array (Long.Long Long.Signed)"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT32 _ = Right $ Just "Array Int"
        ptype Nothing FieldDescriptorProto_Label_LABEL_REPEATED FieldDescriptorProto_Type_TYPE_SINT64 _ = Right $ Just "Array (Long.Long Long.Signed)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_DOUBLE _ = Right $ Just "Maybe.Maybe Number"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_FLOAT _ = Right $ Just "Maybe.Maybe Float32.Float32"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_INT64 _ = Right $ Just "Maybe.Maybe (Long.Long Long.Signed)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_UINT64 _ = Right $ Just "Maybe.Maybe (Long.Long Long.Unsigned)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_INT32 _ = Right $ Just "Maybe.Maybe Int"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_FIXED64 _ = Right $ Just "Maybe.Maybe (Long.Long Long.Unsigned)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_FIXED32 _ = Right $ Just "Maybe.Maybe UInt.UInt"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_BOOL _ = Right $ Just "Maybe.Maybe Boolean"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_STRING _ = Right $ Just "Maybe.Maybe String"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_MESSAGE (Just tname) = Right $ Just $ "Maybe.Maybe " <> mkFieldType "" tname
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_MESSAGE _ = Left "Failed genFieldRecord missing FieldDescriptorProto type_name"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_BYTES _ = Right $ Just "Maybe.Maybe Common.Bytes"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_UINT32 _ = Right $ Just "Maybe.Maybe UInt.UInt"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_ENUM (Just tname) = Right $ Just $ "Maybe.Maybe " <> mkFieldType "" tname
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_ENUM _ = Left "Failed genFieldRecord missing FieldDescriptorProto type_name"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_SFIXED32 _ = Right $ Just "Maybe.Maybe Int"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_SFIXED64 _ = Right $ Just "Maybe.Maybe (Long.Long Long.Signed)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_SINT32 _ = Right $ Just "Maybe.Maybe Int"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_SINT64 _ = Right $ Just "Maybe.Maybe (Long.Long Long.Signed)"
        ptype Nothing _ FieldDescriptorProto_Type_TYPE_GROUP _ = Left "Failed genFieldRecord GROUP not supported"
        ptype (Just _) _ _ _ = Right Nothing -- It's a Oneof, and that case is handled separately
      genFieldRecord _ _ = Left "Failed genFieldRecord missing FieldDescriptorProtocol name or number or label or type"

-- | We need to wrap our structural record types in a nominal
-- | data type so that we can nest records, otherwise we get
-- | https://github.com/purescript/documentation/blob/master/errors/CycleInTypeSynonym.md
-- | And so that we can assign instances.
  let genMessage :: ScopedMsg -> Resp String
      genMessage (ScopedMsg nameSpace (DescriptorProto {name: Just msgName, field, oneof_decl})) =
        let tname = mkTypeName $ nameSpace <> [msgName]
        in
        map (String.joinWith "\n") $ sequence
          [ Right $ "\ntype " <> tname <> "Row ="
          , Right "  ( "
            <>
            ( map (String.joinWith "\n  , ") $
                (  (catMaybes <$> traverse (genFieldRecord nameSpace) field)
                <> (traverse (genFieldRecordOneof (nameSpace <> [msgName])) oneof_decl)
                <> Right ["__unknown_fields :: Array Runtime.UnknownField"]
                )
            )
          , Right "  )"
          , Right $ "type " <> tname <> "R = Record " <> tname <> "Row"
          , Right $ "newtype " <> tname <> " = " <> tname <> " " <> tname <> "R"
          , Right $ "derive instance generic" <> tname <> " :: Generic.Rep.Generic " <> tname <> " _"
          , Right $ "derive instance newtype" <> tname <> " :: Newtype.Newtype " <> tname <> " _"
          , Right $ "derive instance eq" <> tname <> " :: Eq.Eq " <> tname
          -- https://github.com/purescript/purescript/issues/2975#issuecomment-313650710
          , Right $ "instance show" <> tname <> " :: Show.Show " <> tname <> " where show x = Generic.Rep.Show.genericShow x"
          , Right ""
          , Right $ "put" <> tname <> " :: forall m. MonadEffect m => " <> tname <> " -> ArrayBuffer.Builder.PutM m Unit.Unit"
          , Right $ "put" <> tname <> " (" <> tname <> " r) = do"
          , map (String.joinWith "\n") $
              (map catMaybes $ traverse (genFieldPut nameSpace) field) <> (sequence $ Array.mapWithIndex (genOneofPut (nameSpace <> [msgName]) field) oneof_decl)
          , Right "  Traversable.traverse_ Runtime.putFieldUnknown r.__unknown_fields"
          , Right ""
          , Right $ "parse" <> tname <> " :: forall m. MonadEffect m => MonadRec m => Int -> Parser.ParserT ArrayBuffer.Types.DataView m " <> tname
          , Right $ "parse" <> tname <> " length = Runtime.label \"" <> msgName <> " / \" $"
          , Right $ "  Runtime.parseMessage " <> tname <> " default" <> tname <> " parseField length"
          , Right " where"
          , Right "  parseField"
          , Right "    :: Runtime.FieldNumberInt"
          , Right "    -> Common.WireType"
          , Right $ "    -> Parser.ParserT ArrayBuffer.Types.DataView m (Record.Builder.Builder " <> tname <> "R " <> tname <> "R)"
          , map (String.joinWith "\n") (traverse (genFieldParser (nameSpace <> [msgName]) oneof_decl) field)
          , Right "  parseField fieldNumber wireType = Runtime.parseFieldUnknown fieldNumber wireType"
          , Right ""
          , Right $ "default" <> tname <> " :: " <> tname <> "R"
          , Right $ "default" <> tname <> " ="
          , Right "  { "
            <>
            ( map (String.joinWith "\n  , ")
              (  (map catMaybes $ traverse genFieldDefault field)
              <> (traverse genFieldDefaultOneof oneof_decl)
              <> Right ["__unknown_fields: []"]
              )
            )
          , Right "  }"
          , Right ""
          , Right $ "mk" <> tname <> " :: forall r1 r3. Prim.Row.Union r1 " <> tname <> "Row r3 => Prim.Row.Nub r3 " <> tname <> "Row => Record r1 -> " <> tname
          , Right $ "mk" <> tname <> " r = " <> tname <> " $ Record.merge r default" <> tname
          , map (String.joinWith "\n")
            $ (sequence $ (Array.mapWithIndex (genTypeOneof (nameSpace <> [msgName]) field) oneof_decl))
            <> (sequence $ (Array.mapWithIndex (genIsDefaultOneof (nameSpace <> [msgName]) field) oneof_decl))
            <> (sequence $ (Array.mapWithIndex (genOneofMerge (nameSpace <> [msgName]) field) oneof_decl))
          , Right $ "merge" <> tname <> " :: " <> tname <> " -> " <> tname <> " -> " <> tname
          , Right $ "merge" <> tname <> " (" <> tname <> " l) (" <> tname <> " r) = " <> tname
          , Right "  { "
            <>
            (map (String.joinWith "\n  , ")
              (  (map catMaybes $ traverse genFieldMerge field)
              <> (traverseWithIndex (genFieldMergeOneof (nameSpace <> [msgName]) field) oneof_decl)
              <> Right ["__unknown_fields: r.__unknown_fields <> l.__unknown_fields"]
              )
            )
          , Right "  }"
          , Right ""
          ]
      genMessage _ = Left "Failed genMessage no DescriptorProto name"


  contents <-
    sequence
    [ Right $ "-- | Generated by __purescript-protobuf__ from file `" <> fromMaybe "<unknown>" fileName <> "`"
    , Right $ "module " <> (String.joinWith "." ((map mkModuleName packageName))) <> "." <> mkModuleName baseName
    , Right "( " <> (map (String.joinWith "\n, ") ((traverse genMessageExport messages) <> (traverse genEnumExport enums)))
    , Right """)
where

import Prelude
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Alt as Alt
import Record.Builder as Record.Builder
import Data.Array as Array
import Data.Bounded as Bounded
import Data.Enum as Enum
import Data.Eq as Eq
import Data.Function as Function
import Data.Float32 as Float32
import Data.Show as Show
import Data.Ord as Ord
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.Generic.Rep as Generic.Rep
import Data.Generic.Rep.Show as Generic.Rep.Show
import Data.Generic.Rep.Bounded as Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Generic.Rep.Enum
import Data.Generic.Rep.Ord as Generic.Rep.Ord
import Data.Semigroup as Semigroup
import Data.Semiring as Semiring
import Data.String as String
import Data.Symbol as Symbol
import Record as Record
import Data.Traversable as Traversable
import Data.Tuple as Tuple
import Data.UInt as UInt
import Data.Unit as Unit
import Prim.Row as Prim.Row
import Data.Long.Internal as Long
import Text.Parsing.Parser as Parser
import Data.ArrayBuffer.Builder as ArrayBuffer.Builder
import Data.ArrayBuffer.Types as ArrayBuffer.Types
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Protobuf.Common as Common
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Runtime as Runtime
"""
    ]
    <>
    (traverse genImport dependency)
    <>
    Right ["\n"]
    <>
    (traverse genMessage messages)
    <>
    (traverse genEnum enums)
    <>
    Right ["\n"]


  Right $ CodeGeneratorResponse_File
    { name : Just fileNameOut
    , insertion_point : Nothing
    , content : Just $ String.joinWith "\n" contents
    , generated_code_info: Nothing
    , __unknown_fields : []
    }


 where

  mkTypeName :: Array String -> String
  mkTypeName ns = String.joinWith "_" $ map capitalize ns

  capitalize :: String -> String
  capitalize s = String.toUpper (String.take 1 s) <> String.drop 1 s

  decapitalize :: String -> String
  decapitalize s = String.toLower (String.take 1 s) <> String.drop 1 s

  -- | underscores and primes are not allowed in module names
  -- | https://github.com/purescript/documentation/blob/master/errors/ErrorParsingModule.md
  mkModuleName :: String -> String
  mkModuleName n =  capitalize $ illegalDelete $ underscoreToUpper n
   where
    underscoreToUpper :: String -> String
    underscoreToUpper = case String.Regex.regex "_([a-z])" flag of
      Left _ -> identity
      Right patt -> String.Regex.replace' patt toUpper
    toUpper _ [x] = String.toUpper x
    toUpper x _ = x
    flag = String.Regex.Flags.RegexFlags
      { global: true
      , ignoreCase: false
      , multiline: false
      , sticky: false
      , unicode: true
      }
    illegalDelete :: String -> String
    illegalDelete =
      String.replaceAll (String.Pattern.Pattern "_") (String.Pattern.Replacement "") <<<
      String.replaceAll (String.Pattern.Pattern "'") (String.Pattern.Replacement "1")

  -- | Pull all of the enums out of of the nested messages and bring them
  -- | to the top, with their namespace.
  flattenEnums :: NameSpace -> Array DescriptorProto -> Array (Resp ScopedEnum)
  flattenEnums namespace msgarray = concatMap go msgarray
   where
    go :: DescriptorProto -> Array (Resp ScopedEnum)
    go (DescriptorProto {name: Just msgName, nested_type, enum_type: msgEnums}) =
      (Right <$> ScopedEnum (namespace <> [msgName]) <$> msgEnums)
        <> flattenEnums (namespace <> [msgName]) nested_type
    go _ = [ Left "Failed flattenEnums missing DescriptorProto name" ]

  genEnumExport :: ScopedEnum -> Resp String
  genEnumExport (ScopedEnum namespace (EnumDescriptorProto {name: Just eName})) =
    Right $ (mkTypeName $ namespace <> [eName]) <> "(..)"
  genEnumExport _ = Left "Failed genEnumExport missing EnumDescriptorProto name"

  genEnum :: ScopedEnum -> Resp String
  genEnum (ScopedEnum namespace (EnumDescriptorProto {name: Just eName, value})) =
    do
      let tname = mkTypeName $ namespace <> [eName]
      enumConstruct <- traverse genEnumConstruct value
      enumTo <- traverse genEnumTo value
      enumFrom <- traverse genEnumFrom value
      Right $ String.joinWith "\n" $
        [ "\ndata " <> tname
        , "  = " <> String.joinWith "\n  | " enumConstruct
        , "derive instance generic" <> tname <> " :: Generic.Rep.Generic " <> tname <> " _"
        , "derive instance eq" <> tname <> " :: Eq.Eq " <> tname
        , "instance show" <> tname <> " :: Show.Show " <> tname <> " where show = Generic.Rep.Show.genericShow"
        , "instance ord" <> tname <> " :: Ord.Ord " <> tname <> " where compare = Generic.Rep.Ord.genericCompare"
        , "instance bounded" <> tname <> " :: Bounded.Bounded " <> tname
        , " where"
        , "  bottom = Generic.Rep.Bounded.genericBottom"
        , "  top = Generic.Rep.Bounded.genericTop"
        , "instance enum" <> tname <> " :: Enum.Enum " <> tname
        , " where"
        , "  succ = Generic.Rep.Enum.genericSucc"
        , "  pred = Generic.Rep.Enum.genericPred"
        , "instance boundedenum" <> tname <> " :: Enum.BoundedEnum " <> tname
        , " where"
        , "  cardinality = Generic.Rep.Enum.genericCardinality"
        ]
        <>
        enumTo
        <>
        [ "  toEnum _ = Maybe.Nothing"]
        <>
        enumFrom
   where
    genEnumConstruct (EnumValueDescriptorProto {name: Just name}) = Right $ mkEnumName name
    genEnumConstruct arg = Left $ "Failed genEnumConstruct\n" <> show arg
    genEnumTo (EnumValueDescriptorProto {name: Just name,number: Just number}) =
       Right $ "  toEnum " <> show number <> " = Maybe.Just " <> mkEnumName name
    genEnumTo arg = Left $ "Failed genEnumTo\n" <> show arg
    genEnumFrom (EnumValueDescriptorProto {name: Just name,number: Just number}) =
      Right $ "  fromEnum " <> mkEnumName name <> " = " <> show number
    genEnumFrom arg = Left $ "Failed genEnumFrom\n" <> show arg
    mkEnumName name = mkTypeName $ namespace <> [eName] <> [name]
  genEnum _ = Left $ "Failed genEnum no EnumDescriptorProto name"

  -- | Pull all of the nested messages out of of the messages and bring them
  -- | to the top, with their namespace.
  flattenMessages :: NameSpace -> Array DescriptorProto -> Array (Resp ScopedMsg)
  flattenMessages namespace msgarray = concatMap go msgarray
   where
    go :: DescriptorProto -> Array (Resp ScopedMsg)
    go (DescriptorProto r@{name: Just msgName, nested_type}) =
      [Right $ ScopedMsg namespace (DescriptorProto r)]
        <> flattenMessages (namespace <> [msgName]) nested_type
    go _ = [ Left "Failed flattenMessages missing DescriptorProto name"]

  genMessageExport :: ScopedMsg -> Resp String
  genMessageExport (ScopedMsg namespace (DescriptorProto {name: Just msgName, oneof_decl})) = (Right $
    tname <> "(..), " <> tname <> "Row, " <> tname <> "R, parse" <> tname <> ", put" <> tname <> ", default" <> tname <> ", mk" <> tname <> ", merge" <> tname
    )
    <>
    (map (String.joinWith "") (traverse genOneofExport oneof_decl))
   where
    tname = mkTypeName $ namespace <> [msgName]
    genOneofExport (OneofDescriptorProto {name: Just oname}) = Right $ ", " <> mkTypeName (namespace <> [msgName,oname]) <> "(..)"
    genOneofExport _ = Left "Failed genMessageExport missing OneofDescriptorProto name" -- error, no oname
  genMessageExport _ = Left "Failed genMessageExport missing DescriptorProto name" -- error, no name

  -- https://developers.google.com/protocol-buffers/docs/proto3#oneof_features
  -- “A oneof cannot be repeated.”
  genFieldDefaultOneof :: OneofDescriptorProto -> Resp String
  genFieldDefaultOneof (OneofDescriptorProto {name: Just fname}) = Right $ decapitalize fname <> ": Maybe.Nothing"
  genFieldDefaultOneof _ = Left "Failed genFieldDefaultOneof missing name"

  -- https://developers.google.com/protocol-buffers/docs/proto3#oneof_features
  -- “A oneof cannot be repeated.”
  genFieldRecordOneof :: NameSpace -> OneofDescriptorProto -> Resp String
  genFieldRecordOneof nameSpace (OneofDescriptorProto {name: Just fname}) = Right $
    decapitalize fname <> " :: Maybe.Maybe " <> (String.joinWith "_" $ map capitalize $ nameSpace <> [fname])
  genFieldRecordOneof _ _ = Left "Failed genFieldRecordOneof missing OneofDescriptorProto name"

  genFieldDefault :: FieldDescriptorProto -> Resp (Maybe String)
  genFieldDefault (FieldDescriptorProto
    { name: Just name'
    , label: Just flabel
    , oneof_index
    }) = Right $ (\x -> fname <> ": " <> x) <$> dtype oneof_index flabel
   where
    fname = decapitalize name'
    dtype Nothing FieldDescriptorProto_Label_LABEL_REPEATED = Just "[]"
    dtype Nothing _              = Just "Maybe.Nothing"
    dtype (Just _) _ = Nothing -- It's a Oneof so skip it, this case it handled separately.
  genFieldDefault _ = Left "Failed genFieldDefault missing FieldDescriptorProto name or label"
