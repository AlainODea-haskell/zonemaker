module MachineJSON
   ( machineJSON
   , ip
   , MachineSpec( MachineSpec )
   , MachineProp( Brand
                , ImageUUID
                , MaxPhysicalMemory
                , Hostname
                , DNSDomain
                , Resolvers
                , Alias
                , NICs
                , CustomerMetadata )
   , BrandValue ( Joyent
                , KVM )
   , IPAddress
   , NIC( NIC )
   , NICProp( NICTag
            , Gateway
            , IP
            , Netmask )
   , CustomerMetadataProp( RootAuthorizedKeys
                         , UserScript )
   , RootAuthorizedKey( RootAuthorizedKey )
   ) where

import           Data.Bits      (Bits, shiftL, shiftR, (.&.))
import           Data.String    (IsString (fromString))
import           Data.Text      (Text, intercalate, pack)
import qualified Data.Text.IO   as T
import           Data.Aeson.Types (ToJSON(toJSON), (.=), Value, object)
import           Network.Socket (HostAddress)
import Data.Char (toLower)

machineJSON :: MachineSpec -> Value
machineJSON (MachineSpec properties) =
    object $ map toPair properties
    where toPair (Brand x)             = "brand" .= x
          toPair (ImageUUID x)         = "image_uuid" .= x
          toPair (MaxPhysicalMemory x) = "max_physical_memory" .= x
          toPair (Hostname x)          = "hostname" .= x
          toPair (DNSDomain x)         = "dns_domain" .= x
          toPair (Resolvers x)         = "resolvers" .= x
          toPair (Alias x)             = "alias" .= x
          toPair (NICs x)              = "nics" .= x
          toPair (CustomerMetadata x)  =
              "customer_metadata" .= (object $ map metaPair x)
          metaPair (RootAuthorizedKeys xs) =
              "root_authorized_keys" .= intercalate "\n" (map unbox xs)
          metaPair (UserScript x) =
              "user_script" .= x
          unbox (RootAuthorizedKey x) = x

newtype MachineSpec = MachineSpec [MachineProp]
data MachineProp =
      Brand BrandValue
    | ImageUUID Text
    | MaxPhysicalMemory Int
    | Hostname Text
    | DNSDomain Text
    | Resolvers [IPAddress]
    | Alias Text
    | NICs [NIC]
    | CustomerMetadata [CustomerMetadataProp]

data BrandValue =
      Joyent
    | KVM 
    deriving Show
instance ToJSON BrandValue where
    toJSON = toJSON . map toLower . show

newtype IPAddress = IPAddress HostAddress
    deriving (Num, Eq, Data.Bits.Bits)

newtype NIC = NIC [NICProp]
data NICProp =
      NICTag Text
    | Gateway IPAddress
    | IP IPAddress
    | Netmask IPAddress

data CustomerMetadataProp =
      RootAuthorizedKeys [RootAuthorizedKey]
    | UserScript Text
newtype RootAuthorizedKey = RootAuthorizedKey Text

ip :: (Num a, Bits a) => a -> a -> a -> a -> a
ip oct1 oct2 oct3 oct4 = oct1 `shiftL` 24 + oct2 `shiftL` 16 + oct3 `shiftL` 8 + oct4

instance Show IPAddress where
    show (IPAddress x) = show x

instance ToJSON IPAddress where
    toJSON x = toJSON $ intercalate "." $ map (showByte x) [0..3]
      where showByte x n = pack . show $ byte x n
            byte :: IPAddress -> Int -> IPAddress
            byte x n | n == 0 = x `shiftR` 24
                     | n == 1 = (x .&. 255 `shiftL` 16) `shiftR` 16
                     | n == 2 = (x .&. 255 `shiftL` 8) `shiftR` 8
                     | n == 3 = x .&. 255
                     | otherwise = 0

instance ToJSON NIC where
    toJSON (NIC props) = object $ map toPair props
      where toPair (NICTag nicTag)   = "nic_tag" .= nicTag
            toPair (Gateway gateway) = "gateway" .= gateway
            toPair (IP ip)           = "ip" .= ip
            toPair (Netmask netmask) = "netmask" .= netmask
