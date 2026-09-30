{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from messages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Messages_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
aggregatedPayloadSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "aggregatedPayloadSize" a) =>
  Lens.Family2.LensLike' f s a
aggregatedPayloadSize
  = Data.ProtoLens.Field.field @"aggregatedPayloadSize"
backoffMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "backoffMs" a) =>
  Lens.Family2.LensLike' f s a
backoffMs = Data.ProtoLens.Field.field @"backoffMs"
body ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "body" a) =>
  Lens.Family2.LensLike' f s a
body = Data.ProtoLens.Field.field @"body"
code ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "code" a) =>
  Lens.Family2.LensLike' f s a
code = Data.ProtoLens.Field.field @"code"
command ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "command" a) =>
  Lens.Family2.LensLike' f s a
command = Data.ProtoLens.Field.field @"command"
compressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "compressed" a) =>
  Lens.Family2.LensLike' f s a
compressed = Data.ProtoLens.Field.field @"compressed"
cpuUtilization ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "cpuUtilization" a) =>
  Lens.Family2.LensLike' f s a
cpuUtilization = Data.ProtoLens.Field.field @"cpuUtilization"
expectCompressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "expectCompressed" a) =>
  Lens.Family2.LensLike' f s a
expectCompressed = Data.ProtoLens.Field.field @"expectCompressed"
fillGrpclbRouteType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fillGrpclbRouteType" a) =>
  Lens.Family2.LensLike' f s a
fillGrpclbRouteType
  = Data.ProtoLens.Field.field @"fillGrpclbRouteType"
fillOauthScope ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fillOauthScope" a) =>
  Lens.Family2.LensLike' f s a
fillOauthScope = Data.ProtoLens.Field.field @"fillOauthScope"
fillServerId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fillServerId" a) =>
  Lens.Family2.LensLike' f s a
fillServerId = Data.ProtoLens.Field.field @"fillServerId"
fillUsername ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fillUsername" a) =>
  Lens.Family2.LensLike' f s a
fillUsername = Data.ProtoLens.Field.field @"fillUsername"
grpcCodeToReturn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "grpcCodeToReturn" a) =>
  Lens.Family2.LensLike' f s a
grpcCodeToReturn = Data.ProtoLens.Field.field @"grpcCodeToReturn"
grpcStatusDescription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "grpcStatusDescription" a) =>
  Lens.Family2.LensLike' f s a
grpcStatusDescription
  = Data.ProtoLens.Field.field @"grpcStatusDescription"
grpclbRouteType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "grpclbRouteType" a) =>
  Lens.Family2.LensLike' f s a
grpclbRouteType = Data.ProtoLens.Field.field @"grpclbRouteType"
hostname ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hostname" a) =>
  Lens.Family2.LensLike' f s a
hostname = Data.ProtoLens.Field.field @"hostname"
intervalUs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "intervalUs" a) =>
  Lens.Family2.LensLike' f s a
intervalUs = Data.ProtoLens.Field.field @"intervalUs"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
maxReconnectBackoffMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxReconnectBackoffMs" a) =>
  Lens.Family2.LensLike' f s a
maxReconnectBackoffMs
  = Data.ProtoLens.Field.field @"maxReconnectBackoffMs"
maybe'compressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'compressed" a) =>
  Lens.Family2.LensLike' f s a
maybe'compressed = Data.ProtoLens.Field.field @"maybe'compressed"
maybe'expectCompressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'expectCompressed" a) =>
  Lens.Family2.LensLike' f s a
maybe'expectCompressed
  = Data.ProtoLens.Field.field @"maybe'expectCompressed"
maybe'orcaOobReport ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'orcaOobReport" a) =>
  Lens.Family2.LensLike' f s a
maybe'orcaOobReport
  = Data.ProtoLens.Field.field @"maybe'orcaOobReport"
maybe'orcaPerQueryReport ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'orcaPerQueryReport" a) =>
  Lens.Family2.LensLike' f s a
maybe'orcaPerQueryReport
  = Data.ProtoLens.Field.field @"maybe'orcaPerQueryReport"
maybe'payload ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'payload" a) =>
  Lens.Family2.LensLike' f s a
maybe'payload = Data.ProtoLens.Field.field @"maybe'payload"
maybe'responseCompressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'responseCompressed" a) =>
  Lens.Family2.LensLike' f s a
maybe'responseCompressed
  = Data.ProtoLens.Field.field @"maybe'responseCompressed"
maybe'responseStatus ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'responseStatus" a) =>
  Lens.Family2.LensLike' f s a
maybe'responseStatus
  = Data.ProtoLens.Field.field @"maybe'responseStatus"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
memoryUtilization ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "memoryUtilization" a) =>
  Lens.Family2.LensLike' f s a
memoryUtilization = Data.ProtoLens.Field.field @"memoryUtilization"
message ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "message" a) =>
  Lens.Family2.LensLike' f s a
message = Data.ProtoLens.Field.field @"message"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
metadataKeys ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadataKeys" a) =>
  Lens.Family2.LensLike' f s a
metadataKeys = Data.ProtoLens.Field.field @"metadataKeys"
metadatasByPeer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadatasByPeer" a) =>
  Lens.Family2.LensLike' f s a
metadatasByPeer = Data.ProtoLens.Field.field @"metadatasByPeer"
numFailures ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numFailures" a) =>
  Lens.Family2.LensLike' f s a
numFailures = Data.ProtoLens.Field.field @"numFailures"
numRpcs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "numRpcs" a) =>
  Lens.Family2.LensLike' f s a
numRpcs = Data.ProtoLens.Field.field @"numRpcs"
numRpcsFailedByMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numRpcsFailedByMethod" a) =>
  Lens.Family2.LensLike' f s a
numRpcsFailedByMethod
  = Data.ProtoLens.Field.field @"numRpcsFailedByMethod"
numRpcsStartedByMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numRpcsStartedByMethod" a) =>
  Lens.Family2.LensLike' f s a
numRpcsStartedByMethod
  = Data.ProtoLens.Field.field @"numRpcsStartedByMethod"
numRpcsSucceededByMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numRpcsSucceededByMethod" a) =>
  Lens.Family2.LensLike' f s a
numRpcsSucceededByMethod
  = Data.ProtoLens.Field.field @"numRpcsSucceededByMethod"
oauthScope ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oauthScope" a) =>
  Lens.Family2.LensLike' f s a
oauthScope = Data.ProtoLens.Field.field @"oauthScope"
orcaOobReport ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "orcaOobReport" a) =>
  Lens.Family2.LensLike' f s a
orcaOobReport = Data.ProtoLens.Field.field @"orcaOobReport"
orcaPerQueryReport ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "orcaPerQueryReport" a) =>
  Lens.Family2.LensLike' f s a
orcaPerQueryReport
  = Data.ProtoLens.Field.field @"orcaPerQueryReport"
passed ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "passed" a) =>
  Lens.Family2.LensLike' f s a
passed = Data.ProtoLens.Field.field @"passed"
payload ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "payload" a) =>
  Lens.Family2.LensLike' f s a
payload = Data.ProtoLens.Field.field @"payload"
requestCost ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestCost" a) =>
  Lens.Family2.LensLike' f s a
requestCost = Data.ProtoLens.Field.field @"requestCost"
responseCompressed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseCompressed" a) =>
  Lens.Family2.LensLike' f s a
responseCompressed
  = Data.ProtoLens.Field.field @"responseCompressed"
responseParameters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseParameters" a) =>
  Lens.Family2.LensLike' f s a
responseParameters
  = Data.ProtoLens.Field.field @"responseParameters"
responseSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseSize" a) =>
  Lens.Family2.LensLike' f s a
responseSize = Data.ProtoLens.Field.field @"responseSize"
responseStatus ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseStatus" a) =>
  Lens.Family2.LensLike' f s a
responseStatus = Data.ProtoLens.Field.field @"responseStatus"
responseType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseType" a) =>
  Lens.Family2.LensLike' f s a
responseType = Data.ProtoLens.Field.field @"responseType"
result ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "result" a) =>
  Lens.Family2.LensLike' f s a
result = Data.ProtoLens.Field.field @"result"
rpcMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "rpcMetadata" a) =>
  Lens.Family2.LensLike' f s a
rpcMetadata = Data.ProtoLens.Field.field @"rpcMetadata"
rpcsByMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "rpcsByMethod" a) =>
  Lens.Family2.LensLike' f s a
rpcsByMethod = Data.ProtoLens.Field.field @"rpcsByMethod"
rpcsByPeer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "rpcsByPeer" a) =>
  Lens.Family2.LensLike' f s a
rpcsByPeer = Data.ProtoLens.Field.field @"rpcsByPeer"
rpcsStarted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "rpcsStarted" a) =>
  Lens.Family2.LensLike' f s a
rpcsStarted = Data.ProtoLens.Field.field @"rpcsStarted"
rss ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "rss" a) =>
  Lens.Family2.LensLike' f s a
rss = Data.ProtoLens.Field.field @"rss"
serverId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverId" a) =>
  Lens.Family2.LensLike' f s a
serverId = Data.ProtoLens.Field.field @"serverId"
serverPort ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverPort" a) =>
  Lens.Family2.LensLike' f s a
serverPort = Data.ProtoLens.Field.field @"serverPort"
size ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "size" a) =>
  Lens.Family2.LensLike' f s a
size = Data.ProtoLens.Field.field @"size"
statsPerMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "statsPerMethod" a) =>
  Lens.Family2.LensLike' f s a
statsPerMethod = Data.ProtoLens.Field.field @"statsPerMethod"
timeoutSec ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timeoutSec" a) =>
  Lens.Family2.LensLike' f s a
timeoutSec = Data.ProtoLens.Field.field @"timeoutSec"
type' ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "type'" a) =>
  Lens.Family2.LensLike' f s a
type' = Data.ProtoLens.Field.field @"type'"
types ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "types" a) =>
  Lens.Family2.LensLike' f s a
types = Data.ProtoLens.Field.field @"types"
username ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "username" a) =>
  Lens.Family2.LensLike' f s a
username = Data.ProtoLens.Field.field @"username"
utilization ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "utilization" a) =>
  Lens.Family2.LensLike' f s a
utilization = Data.ProtoLens.Field.field @"utilization"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'backoffMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'backoffMs" a) =>
  Lens.Family2.LensLike' f s a
vec'backoffMs = Data.ProtoLens.Field.field @"vec'backoffMs"
vec'metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metadata" a) =>
  Lens.Family2.LensLike' f s a
vec'metadata = Data.ProtoLens.Field.field @"vec'metadata"
vec'metadataKeys ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metadataKeys" a) =>
  Lens.Family2.LensLike' f s a
vec'metadataKeys = Data.ProtoLens.Field.field @"vec'metadataKeys"
vec'responseParameters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'responseParameters" a) =>
  Lens.Family2.LensLike' f s a
vec'responseParameters
  = Data.ProtoLens.Field.field @"vec'responseParameters"
vec'rpcMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'rpcMetadata" a) =>
  Lens.Family2.LensLike' f s a
vec'rpcMetadata = Data.ProtoLens.Field.field @"vec'rpcMetadata"
vec'types ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'types" a) =>
  Lens.Family2.LensLike' f s a
vec'types = Data.ProtoLens.Field.field @"vec'types"