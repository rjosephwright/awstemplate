{-# LANGUAGE OverloadedStrings #-}

module Data.IP.Extra where

import Data.IP

-- Return a List of AddrRanges within a given AddrRange
-- that have the given netmask. If the given netmask is
-- greater than the netmask of the original AddrRange,
-- an empty List is returned.
ranges :: AddrRange IPv4 -> Int -> [AddrRange IPv4]
ranges addrRange mask
  | m > mask = []
  | otherwise = [makeAddrRange (intToIPv4 i) mask | i <- [first,first+step..last]]
  where
    (r, m) = addrRangePair addrRange
    first = iPv4ToInt r
    last = first+(2^(32-m))-1
    step = 2^(32-mask)

iPv4ToInt :: IPv4 -> Int
iPv4ToInt i =
  let (o1:o2:o3:o4:_) = fromIPv4 i
      oct n pow = n*((256::Int)^(pow::Int)) in
  (oct o1 3) + (oct o2 2) + (oct o3 1) + o4

intToIPv4 :: Int -> IPv4
intToIPv4 i =
  let (i', o4) = i `divMod` 256
      (i'', o3) = i' `divMod` 256
      (o1, o2) = i'' `divMod` 256 in
  toIPv4 [o1, o2, o3, o4]
