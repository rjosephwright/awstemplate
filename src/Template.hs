{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Template where

import Data.IP
import Data.IP.Extra
import Data.Set
import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLI
import Control.Lens
import Control.Monad.State

data ContextState = ContextState {
  _vpcCidrBlock :: AddrRange IPv4,
  _usedCidrBlocks :: Set (AddrRange IPv4),
  _netmask :: Int
} deriving Show

makeLenses ''ContextState

initialState :: AddrRange IPv4 -> Int -> ContextState
initialState cb netmask = ContextState {
  _vpcCidrBlock = cb,
  _usedCidrBlocks = empty,
  _netmask = netmask
}

templateMain :: IO ()
templateMain = run >>= TLI.putStrLn

run :: IO TL.Text
run = evalStateT stateFunc (initialState "172.31.0.0/16" 24)

stateFunc :: StateT ContextState IO TL.Text
stateFunc = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)

template :: String
template = "{{#next_cidr_block}}{{/next_cidr_block}} {{#next_cidr_block}}{{/next_cidr_block}} {{#next_cidr_block}}{{/next_cidr_block}}"

context :: (MonadState ContextState m) => String -> MuType m
context "next_cidr_block" = MuLambdaM $ nextCidrBlock . decodeStr

nextCidrBlock :: (MonadState ContextState m) => String -> m String
nextCidrBlock _ = do
  st <- get
  let cb = st^.vpcCidrBlock
  let ucb = st^.usedCidrBlocks
  let mask = st^.netmask
  let next = getNext cb ucb mask
  usedCidrBlocks .= insert next ucb
  return (show next)

getNext :: AddrRange IPv4 -> Set (AddrRange IPv4) -> Int -> AddrRange IPv4
getNext cb ucb netmask =
  let diff = difference (fromList $ ranges cb netmask) ucb in
  elemAt 0 diff
