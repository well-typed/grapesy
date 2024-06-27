-- | Utilities for working with Protobuf messages
module Test.Util.Protobuf (
    messageToExpr
  ) where

import Data.ProtoLens.Message
import Data.Proxy
import Data.Text qualified as Text
import Data.TreeDiff
import Data.TreeDiff.OMap qualified as OMap
import Control.Lens ((^.), Lens', (.~))
import Data.Map qualified as Map
import Data.Function ((&))
import Data.ProtoLens.Encoding.Wire

{-------------------------------------------------------------------------------
  TreeDiff
-------------------------------------------------------------------------------}

messageToExpr :: forall msg. Message msg => msg -> Expr
messageToExpr =
      Rec (Text.unpack $ messageName (Proxy @msg))
    . OMap.fromList
    . fields

fields :: Message msg => msg -> [(FieldName, Expr)]
fields msg = concat [
      map (knownField msg) allFields
    , map unknownField (msg ^. unknownFields)
    ]

knownField :: msg -> FieldDescriptor msg -> (FieldName, Expr)
knownField msg (FieldDescriptor name typ acc) = (
      name
    , case acc of
        PlainField _    f ->          plainField typ  $  msg ^. f
        OptionalField   f -> toExpr $ plainField typ <$> msg ^. f
        RepeatedField _ f -> toExpr $ plainField typ <$> msg ^. f
        MapField    k v f -> toExpr $
          (messageToExpr . pairToMsg k v) <$> Map.toList (msg ^. f)
    )

plainField :: FieldTypeDescriptor value -> value -> Expr
plainField (MessageField _)  = messageToExpr
plainField (ScalarField typ) = scalarField typ

scalarField :: ScalarField value -> value -> Expr
scalarField EnumField     = enumField
scalarField Int32Field    = toExpr
scalarField Int64Field    = toExpr
scalarField UInt32Field   = toExpr
scalarField UInt64Field   = toExpr
scalarField SInt32Field   = toExpr
scalarField SInt64Field   = toExpr
scalarField Fixed32Field  = toExpr
scalarField Fixed64Field  = toExpr
scalarField SFixed32Field = toExpr
scalarField SFixed64Field = toExpr
scalarField FloatField    = toExpr
scalarField DoubleField   = toExpr
scalarField BoolField     = toExpr
scalarField StringField   = toExpr
scalarField BytesField    = toExpr

enumField :: MessageEnum value => value -> Expr
enumField = toExpr . fromEnum

unknownField :: TaggedValue -> (FieldName, Expr)
unknownField (TaggedValue (Tag tag) value) = (show tag, wireValue value)

wireValue :: WireValue -> Expr
wireValue (Fixed32 x) = toExpr x
wireValue (Fixed64 x) = toExpr x
wireValue (Lengthy x) = toExpr x
wireValue (VarInt x)  = toExpr x
wireValue StartGroup  = toExpr ()
wireValue EndGroup    = toExpr ()

{-------------------------------------------------------------------------------
  Auxiliary: working with maps
-------------------------------------------------------------------------------}

pairToMsg :: Message msg => Lens' msg k -> Lens' msg v -> (k, v) -> msg
pairToMsg k v (x, y) = defMessage & k .~ x & v .~ y
