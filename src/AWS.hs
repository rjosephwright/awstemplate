{-# LANGUAGE OverloadedStrings #-}
module AWS where

import Control.Lens
import qualified Control.Monad.Trans.AWS as T
import Data.Text (Text)
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.Types
import Network.AWS
import System.IO

vpcAttr :: Text -> Getting a Vpc a -> IO (Maybe a)
vpcAttr vpcId attr = do
    l <- newLogger Info stdout
    env <- getEnv NorthCalifornia Discover <&> envLogger .~ l
    r <- T.runAWST env $ do
      vpcs <- T.send $ describeVpcs & dv1VpcIds .~ [vpcId]
      return (vpcs^.dvrVpcs)
    case r of
      (Left _) -> return Nothing
      (Right []) -> return Nothing
      (Right (vpc:_)) -> return $ Just (vpc^.attr)

cidrBlock :: Text -> IO (Maybe Text)
cidrBlock vpcId = vpcAttr vpcId vpcCidrBlock

subnetCidrBlocks :: Text -> IO (Maybe [Text])
subnetCidrBlocks vpcId = subnetsAttr vpcId s1CidrBlock

subnetsAttr :: Text -> Getting a Subnet a -> IO (Maybe [a])
subnetsAttr vpcId attr = do
  l <- newLogger Info stdout
  env <- getEnv NorthCalifornia Discover <&> envLogger .~ l
  r <- T.runAWST env $ do
    sn <- T.send $ describeSubnets & dsFilters .~ [filter' "vpc-id" & fValues .~ [vpcId]]
    return (sn^.dsrSubnets)
  case r of
    (Left _) -> return Nothing
    (Right []) -> return Nothing
    (Right sns) -> return $ Just [sn^.attr | sn <- sns]
