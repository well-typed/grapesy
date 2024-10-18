{- This file was auto-generated from test.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
module Proto.Test (
        TestService(..), UnimplementedService(..), ReconnectService(..),
        LoadBalancerStatsService(..), HookService(..),
        XdsUpdateHealthService(..), XdsUpdateClientConfigureService(..)
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
import qualified Proto.Empty
import qualified Proto.Messages
data TestService = TestService {}
instance Data.ProtoLens.Service.Types.Service TestService where
  type ServiceName TestService = "TestService"
  type ServicePackage TestService = "grpc.testing"
  type ServiceMethods TestService = '["cacheableUnaryCall",
                                      "emptyCall",
                                      "fullDuplexCall",
                                      "halfDuplexCall",
                                      "streamingInputCall",
                                      "streamingOutputCall",
                                      "unaryCall",
                                      "unimplementedCall"]
  packedServiceDescriptor _
    = "\n\
      \\vTestService\DC25\n\
      \\tEmptyCall\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty\DC2F\n\
      \\tUnaryCall\DC2\ESC.grpc.testing.SimpleRequest\SUB\FS.grpc.testing.SimpleResponse\DC2O\n\
      \\DC2CacheableUnaryCall\DC2\ESC.grpc.testing.SimpleRequest\SUB\FS.grpc.testing.SimpleResponse\DC2l\n\
      \\DC3StreamingOutputCall\DC2(.grpc.testing.StreamingOutputCallRequest\SUB).grpc.testing.StreamingOutputCallResponse0\SOH\DC2i\n\
      \\DC2StreamingInputCall\DC2'.grpc.testing.StreamingInputCallRequest\SUB(.grpc.testing.StreamingInputCallResponse(\SOH\DC2i\n\
      \\SOFullDuplexCall\DC2(.grpc.testing.StreamingOutputCallRequest\SUB).grpc.testing.StreamingOutputCallResponse(\SOH0\SOH\DC2i\n\
      \\SO\&HalfDuplexCall\DC2(.grpc.testing.StreamingOutputCallRequest\SUB).grpc.testing.StreamingOutputCallResponse(\SOH0\SOH\DC2=\n\
      \\DC1UnimplementedCall\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty"
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "emptyCall" where
  type MethodName TestService "emptyCall" = "EmptyCall"
  type MethodInput TestService "emptyCall" = Proto.Empty.Empty
  type MethodOutput TestService "emptyCall" = Proto.Empty.Empty
  type MethodStreamingType TestService "emptyCall" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "unaryCall" where
  type MethodName TestService "unaryCall" = "UnaryCall"
  type MethodInput TestService "unaryCall" = Proto.Messages.SimpleRequest
  type MethodOutput TestService "unaryCall" = Proto.Messages.SimpleResponse
  type MethodStreamingType TestService "unaryCall" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "cacheableUnaryCall" where
  type MethodName TestService "cacheableUnaryCall" = "CacheableUnaryCall"
  type MethodInput TestService "cacheableUnaryCall" = Proto.Messages.SimpleRequest
  type MethodOutput TestService "cacheableUnaryCall" = Proto.Messages.SimpleResponse
  type MethodStreamingType TestService "cacheableUnaryCall" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "streamingOutputCall" where
  type MethodName TestService "streamingOutputCall" = "StreamingOutputCall"
  type MethodInput TestService "streamingOutputCall" = Proto.Messages.StreamingOutputCallRequest
  type MethodOutput TestService "streamingOutputCall" = Proto.Messages.StreamingOutputCallResponse
  type MethodStreamingType TestService "streamingOutputCall" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "streamingInputCall" where
  type MethodName TestService "streamingInputCall" = "StreamingInputCall"
  type MethodInput TestService "streamingInputCall" = Proto.Messages.StreamingInputCallRequest
  type MethodOutput TestService "streamingInputCall" = Proto.Messages.StreamingInputCallResponse
  type MethodStreamingType TestService "streamingInputCall" = 'Data.ProtoLens.Service.Types.ClientStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "fullDuplexCall" where
  type MethodName TestService "fullDuplexCall" = "FullDuplexCall"
  type MethodInput TestService "fullDuplexCall" = Proto.Messages.StreamingOutputCallRequest
  type MethodOutput TestService "fullDuplexCall" = Proto.Messages.StreamingOutputCallResponse
  type MethodStreamingType TestService "fullDuplexCall" = 'Data.ProtoLens.Service.Types.BiDiStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "halfDuplexCall" where
  type MethodName TestService "halfDuplexCall" = "HalfDuplexCall"
  type MethodInput TestService "halfDuplexCall" = Proto.Messages.StreamingOutputCallRequest
  type MethodOutput TestService "halfDuplexCall" = Proto.Messages.StreamingOutputCallResponse
  type MethodStreamingType TestService "halfDuplexCall" = 'Data.ProtoLens.Service.Types.BiDiStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl TestService "unimplementedCall" where
  type MethodName TestService "unimplementedCall" = "UnimplementedCall"
  type MethodInput TestService "unimplementedCall" = Proto.Empty.Empty
  type MethodOutput TestService "unimplementedCall" = Proto.Empty.Empty
  type MethodStreamingType TestService "unimplementedCall" = 'Data.ProtoLens.Service.Types.NonStreaming
data UnimplementedService = UnimplementedService {}
instance Data.ProtoLens.Service.Types.Service UnimplementedService where
  type ServiceName UnimplementedService = "UnimplementedService"
  type ServicePackage UnimplementedService = "grpc.testing"
  type ServiceMethods UnimplementedService = '["unimplementedCall"]
  packedServiceDescriptor _
    = "\n\
      \\DC4UnimplementedService\DC2=\n\
      \\DC1UnimplementedCall\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty"
instance Data.ProtoLens.Service.Types.HasMethodImpl UnimplementedService "unimplementedCall" where
  type MethodName UnimplementedService "unimplementedCall" = "UnimplementedCall"
  type MethodInput UnimplementedService "unimplementedCall" = Proto.Empty.Empty
  type MethodOutput UnimplementedService "unimplementedCall" = Proto.Empty.Empty
  type MethodStreamingType UnimplementedService "unimplementedCall" = 'Data.ProtoLens.Service.Types.NonStreaming
data ReconnectService = ReconnectService {}
instance Data.ProtoLens.Service.Types.Service ReconnectService where
  type ServiceName ReconnectService = "ReconnectService"
  type ServicePackage ReconnectService = "grpc.testing"
  type ServiceMethods ReconnectService = '["start", "stop"]
  packedServiceDescriptor _
    = "\n\
      \\DLEReconnectService\DC2;\n\
      \\ENQStart\DC2\GS.grpc.testing.ReconnectParams\SUB\DC3.grpc.testing.Empty\DC28\n\
      \\EOTStop\DC2\DC3.grpc.testing.Empty\SUB\ESC.grpc.testing.ReconnectInfo"
instance Data.ProtoLens.Service.Types.HasMethodImpl ReconnectService "start" where
  type MethodName ReconnectService "start" = "Start"
  type MethodInput ReconnectService "start" = Proto.Messages.ReconnectParams
  type MethodOutput ReconnectService "start" = Proto.Empty.Empty
  type MethodStreamingType ReconnectService "start" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl ReconnectService "stop" where
  type MethodName ReconnectService "stop" = "Stop"
  type MethodInput ReconnectService "stop" = Proto.Empty.Empty
  type MethodOutput ReconnectService "stop" = Proto.Messages.ReconnectInfo
  type MethodStreamingType ReconnectService "stop" = 'Data.ProtoLens.Service.Types.NonStreaming
data LoadBalancerStatsService = LoadBalancerStatsService {}
instance Data.ProtoLens.Service.Types.Service LoadBalancerStatsService where
  type ServiceName LoadBalancerStatsService = "LoadBalancerStatsService"
  type ServicePackage LoadBalancerStatsService = "grpc.testing"
  type ServiceMethods LoadBalancerStatsService = '["getClientAccumulatedStats",
                                                   "getClientStats"]
  packedServiceDescriptor _
    = "\n\
      \\CANLoadBalancerStatsService\DC2c\n\
      \\SOGetClientStats\DC2&.grpc.testing.LoadBalancerStatsRequest\SUB'.grpc.testing.LoadBalancerStatsResponse\"\NUL\DC2\132\SOH\n\
      \\EMGetClientAccumulatedStats\DC21.grpc.testing.LoadBalancerAccumulatedStatsRequest\SUB2.grpc.testing.LoadBalancerAccumulatedStatsResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl LoadBalancerStatsService "getClientStats" where
  type MethodName LoadBalancerStatsService "getClientStats" = "GetClientStats"
  type MethodInput LoadBalancerStatsService "getClientStats" = Proto.Messages.LoadBalancerStatsRequest
  type MethodOutput LoadBalancerStatsService "getClientStats" = Proto.Messages.LoadBalancerStatsResponse
  type MethodStreamingType LoadBalancerStatsService "getClientStats" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl LoadBalancerStatsService "getClientAccumulatedStats" where
  type MethodName LoadBalancerStatsService "getClientAccumulatedStats" = "GetClientAccumulatedStats"
  type MethodInput LoadBalancerStatsService "getClientAccumulatedStats" = Proto.Messages.LoadBalancerAccumulatedStatsRequest
  type MethodOutput LoadBalancerStatsService "getClientAccumulatedStats" = Proto.Messages.LoadBalancerAccumulatedStatsResponse
  type MethodStreamingType LoadBalancerStatsService "getClientAccumulatedStats" = 'Data.ProtoLens.Service.Types.NonStreaming
data HookService = HookService {}
instance Data.ProtoLens.Service.Types.Service HookService where
  type ServiceName HookService = "HookService"
  type ServicePackage HookService = "grpc.testing"
  type ServiceMethods HookService = '["clearReturnStatus",
                                      "hook",
                                      "setReturnStatus"]
  packedServiceDescriptor _
    = "\n\
      \\vHookService\DC20\n\
      \\EOTHook\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty\DC2L\n\
      \\SISetReturnStatus\DC2$.grpc.testing.SetReturnStatusRequest\SUB\DC3.grpc.testing.Empty\DC2=\n\
      \\DC1ClearReturnStatus\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty"
instance Data.ProtoLens.Service.Types.HasMethodImpl HookService "hook" where
  type MethodName HookService "hook" = "Hook"
  type MethodInput HookService "hook" = Proto.Empty.Empty
  type MethodOutput HookService "hook" = Proto.Empty.Empty
  type MethodStreamingType HookService "hook" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl HookService "setReturnStatus" where
  type MethodName HookService "setReturnStatus" = "SetReturnStatus"
  type MethodInput HookService "setReturnStatus" = Proto.Messages.SetReturnStatusRequest
  type MethodOutput HookService "setReturnStatus" = Proto.Empty.Empty
  type MethodStreamingType HookService "setReturnStatus" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl HookService "clearReturnStatus" where
  type MethodName HookService "clearReturnStatus" = "ClearReturnStatus"
  type MethodInput HookService "clearReturnStatus" = Proto.Empty.Empty
  type MethodOutput HookService "clearReturnStatus" = Proto.Empty.Empty
  type MethodStreamingType HookService "clearReturnStatus" = 'Data.ProtoLens.Service.Types.NonStreaming
data XdsUpdateHealthService = XdsUpdateHealthService {}
instance Data.ProtoLens.Service.Types.Service XdsUpdateHealthService where
  type ServiceName XdsUpdateHealthService = "XdsUpdateHealthService"
  type ServicePackage XdsUpdateHealthService = "grpc.testing"
  type ServiceMethods XdsUpdateHealthService = '["sendHookRequest",
                                                 "setNotServing",
                                                 "setServing"]
  packedServiceDescriptor _
    = "\n\
      \\SYNXdsUpdateHealthService\DC26\n\
      \\n\
      \SetServing\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty\DC29\n\
      \\rSetNotServing\DC2\DC3.grpc.testing.Empty\SUB\DC3.grpc.testing.Empty\DC2H\n\
      \\SISendHookRequest\DC2\EM.grpc.testing.HookRequest\SUB\SUB.grpc.testing.HookResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl XdsUpdateHealthService "setServing" where
  type MethodName XdsUpdateHealthService "setServing" = "SetServing"
  type MethodInput XdsUpdateHealthService "setServing" = Proto.Empty.Empty
  type MethodOutput XdsUpdateHealthService "setServing" = Proto.Empty.Empty
  type MethodStreamingType XdsUpdateHealthService "setServing" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl XdsUpdateHealthService "setNotServing" where
  type MethodName XdsUpdateHealthService "setNotServing" = "SetNotServing"
  type MethodInput XdsUpdateHealthService "setNotServing" = Proto.Empty.Empty
  type MethodOutput XdsUpdateHealthService "setNotServing" = Proto.Empty.Empty
  type MethodStreamingType XdsUpdateHealthService "setNotServing" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl XdsUpdateHealthService "sendHookRequest" where
  type MethodName XdsUpdateHealthService "sendHookRequest" = "SendHookRequest"
  type MethodInput XdsUpdateHealthService "sendHookRequest" = Proto.Messages.HookRequest
  type MethodOutput XdsUpdateHealthService "sendHookRequest" = Proto.Messages.HookResponse
  type MethodStreamingType XdsUpdateHealthService "sendHookRequest" = 'Data.ProtoLens.Service.Types.NonStreaming
data XdsUpdateClientConfigureService
  = XdsUpdateClientConfigureService {}
instance Data.ProtoLens.Service.Types.Service XdsUpdateClientConfigureService where
  type ServiceName XdsUpdateClientConfigureService = "XdsUpdateClientConfigureService"
  type ServicePackage XdsUpdateClientConfigureService = "grpc.testing"
  type ServiceMethods XdsUpdateClientConfigureService = '["configure"]
  packedServiceDescriptor _
    = "\n\
      \\USXdsUpdateClientConfigureService\DC2X\n\
      \\tConfigure\DC2$.grpc.testing.ClientConfigureRequest\SUB%.grpc.testing.ClientConfigureResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl XdsUpdateClientConfigureService "configure" where
  type MethodName XdsUpdateClientConfigureService "configure" = "Configure"
  type MethodInput XdsUpdateClientConfigureService "configure" = Proto.Messages.ClientConfigureRequest
  type MethodOutput XdsUpdateClientConfigureService "configure" = Proto.Messages.ClientConfigureResponse
  type MethodStreamingType XdsUpdateClientConfigureService "configure" = 'Data.ProtoLens.Service.Types.NonStreaming