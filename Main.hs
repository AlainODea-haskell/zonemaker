import MachineJSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import Shelly
import Prelude hiding (FilePath)
default (T.Text)

main = do
  createTestMachine "tomcat01.alainodea.local" $ ip 192 168 2 22

createTestMachine :: T.Text -> IPAddress -> IO ()
createTestMachine = createMachine classBProdnet users

classBProdnet :: IPAddress -> NIC
classBProdnet addr =
  NIC [
    NICTag "external"
  , Gateway $ ip 192 168  2  1
  , IP addr
  , Netmask $ ip 255 255 255 0
  ]

users :: [RootAuthorizedKey]
users = [
    alain_odea
  ]

alain_odea :: RootAuthorizedKey
alain_odea = RootAuthorizedKey "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTLDnggGlVWHOrlbEpLP8ZCmUQrUwq13+x+e216gghxQi+IZiEfTpQWoFqz5opDVgNzRdBWfMcS6qhBK1YyIt/I+W6tZTsvR3Ncq6XS0EATdSFZWozpgrC5jFnR1fAAoAJCDqcUs7lMd2Fg5MJe3y9PTlcFP+WOxfN/3zwqd1kH0hCJGLZbS3LgB7IaxLQylLwa6MzOvsw/65s24whGfGCW6Fgh/tsC2W2wLzKix6qNg8ypoBYtBZqtx41quDDfg62bDX53CXgqDxiYtUvQGUA7jvALtig8ttAZyZdWbAIgKKwURZjQIFIMMPhpT6eHkSIQ3qXNkh2qvlOmvM4s2LJ alain.odea@gmail.com"

createMachine :: (IPAddress -> NIC) -> [RootAuthorizedKey] -> T.Text -> IPAddress -> IO ()
createMachine nicBuilder keys hostname hostIp = shelly $ do
  setStdin $ T.decodeUtf8 . BL.toStrict . encode . machineJSON $ MachineSpec [
      Brand Joyent
    , ImageUUID imageUuid
    , MaxPhysicalMemory maxMemory
    , Hostname hostname
    , DNSDomain dnsDomain
    , Resolvers resolvers
    , Alias alias
    , NICs [nicBuilder hostIp]
    , CustomerMetadata [
        RootAuthorizedKeys keys
      , UserScript "/usr/sbin/mdata-get root_authorized_keys > ~root/.ssh/authorized_keys ; /usr/sbin/mdata-get root_authorized_keys > ~admin/.ssh/authorized_keys"
      ]
    ]
  run_ "cat" []
--run_ "ssh" ["root@smartos01", "vmadm", "create"]

--resolvers = [ip 192 168 2 2, ip 192 168 2 3]
resolvers = [ ip 208 67 222 222
            , ip 208 67 220 220 ]
imageUuid = "c353c568-69ad-11e3-a248-db288786ea63"
maxMemory = 2048
dnsDomain = "alainodea.local"
alias = "tomcat"
