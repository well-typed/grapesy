-- | gRPC server using Protobuf
--
-- Intended for unqualified import.
module Network.GRPC.Server.Protobuf (
    -- * Compute full Protobuf API
    ProtobufServices
  , ProtobufMethodsOf
  , ProtobufMethods
  ) where

import Data.Kind
import Data.ProtoLens.Service.Types
import GHC.TypeLits

import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Compute full Protobuf API
-------------------------------------------------------------------------------}

type family ProtobufServices (servs :: [Type]) :: [[Type]] where
  ProtobufServices '[]             = '[]
  ProtobufServices (serv ': servs) = ProtobufMethodsOf serv
                                  ': ProtobufServices servs

type family ProtobufMethodsOf (serv :: Type) :: [Type] where
  ProtobufMethodsOf serv = ProtobufMethods serv (ServiceMethods serv)

type family ProtobufMethods (serv :: Type) (methds :: [Symbol]) :: [Type] where
  ProtobufMethods serv '[]             = '[]
  ProtobufMethods serv (meth ': meths) = Protobuf serv meth
                                      ': ProtobufMethods serv meths