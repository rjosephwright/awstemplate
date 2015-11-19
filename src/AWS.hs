{-# LANGUAGE OverloadedStrings #-}
module AWS where

import Control.Lens
import qualified Control.Monad.Trans.AWS as T
import Data.Text (Text)
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.Types
import Network.AWS
import System.IO

vpcAttr :: Text -> Getting a VPC a -> IO (Maybe a)
vpcAttr vpcId attr = do
    l <- newLogger Info stdout
    env <- newEnv NorthCalifornia Discover <&> envLogger .~ l
    r <- T.runResourceT . T.runAWST env $ do
      vpcs <- T.send $ describeVPCs & dvsVPCIds .~ [vpcId]
      return (vpcs^.dvrsVPCs)
    case r of
      [] -> return Nothing
      (vpc:_) -> return $ Just (vpc^.attr)

cidrBlock :: Text -> IO (Maybe Text)
cidrBlock vpcId = vpcAttr vpcId vpcCIdRBlock

subnetCidrBlocks :: Text -> IO (Maybe [Text])
subnetCidrBlocks vpcId = subnetsAttr vpcId subCIdRBlock

subnetsAttr :: Text -> Getting a Subnet a -> IO (Maybe [a])
subnetsAttr vpcId attr = do
  l <- newLogger Info stdout
  env <- newEnv NorthCalifornia Discover <&> envLogger .~ l
  r <- T.runResourceT . T.runAWST env $ do
    sn <- T.send $ describeSubnets & dsFilters .~ [filter' "vpc-id" & fValues .~ [vpcId]]
    return (sn^.dsrsSubnets)
  case r of
    [] -> return Nothing
    sns -> return $ Just [sn^.attr | sn <- sns]
