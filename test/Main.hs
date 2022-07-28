{-# LANGUAGE FlexibleContexts,PatternGuards #-}

module Main where

import Control.Monad
import Data.Array (elems)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (isJust)
import qualified Data.Sequence as S
import System.Exit

import Text.Regex.Posix

check_ :: (Show s, RegexLike Regex s) => String -> s -> [[(Int,Int)]] -> IO Bool
check_ pattern input expected = do
  let compiled = makeRegex pattern :: Regex
  let expectedOnce | [] <- expected = Nothing
                   | otherwise = Just (head expected)
  let expectedTest = isJust expectedOnce
  let expectedCount = length expected

  let actual = map elems $ matchAll compiled input
  let actualOnce = elems <$> matchOnce compiled input
  let actualTest = matchTest compiled input
  let actualCount = matchCount compiled input

  when (actual /= expected) $
    putStrLn ("FAIL: " ++ show pattern ++ " on " ++
              show input ++ "\n" ++
              "Expected: " ++ show expected ++ "\n" ++
              "But got : " ++ show actual)

  if actualOnce /= expectedOnce
    then putStrLn ("FAIL: [matchOnce] " ++ show pattern ++ " on " ++
                   show input ++ "\n" ++
                   "Expected: " ++ show expectedOnce ++ "\n" ++
                   "But got : " ++ show actualOnce)
    else return ()
  if actualTest /= expectedTest
    then putStrLn ("FAIL: [matchTest] " ++ show pattern ++ " on " ++
                   show input ++ "\n" ++
                   "Expected: " ++ show expectedTest ++ "\n" ++
                   "But got : " ++ show actualTest)
    else return ()
  if actualCount /= expectedCount
    then putStrLn ("FAIL: [matchCount] " ++ show pattern ++ " on " ++
                   show input ++ "\n" ++
                   "Expected: " ++ show expectedCount ++ "\n" ++
                   "But got : " ++ show actualCount)
    else return ()

  return (actual == expected && actualOnce == expectedOnce && actualTest == expectedTest && actualCount == expectedCount)

check :: String -> String -> [[(Int,Int)]] -> IO Bool
check pattern input expected = (and <$>) $ sequence $
  [ check_ pattern input expected
  , check_ pattern (L.pack input) expected
  , check_ pattern (C.pack input) expected
  , check_ pattern (S.fromList input) expected
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  results <- sequence tests
  let pass = length (filter id results)
  let fail = length (filter not results)
  putStrLn ("PASS: " ++ show pass)
  putStrLn ("FAIL: " ++ show fail)
  if fail > 0 then exitFailure else exitSuccess

-- This mainly tests basic functionality and embedded NULs. Should add some
-- tests for regex flags as well.
main = do
 runTests $
  [ check "foo" "barfoo" [[(3,3)]]
  , check "foo" "\0\0\0foo" [[(3,3)]]
  , check "foo" "\0\0\0foofoo\0foo" [[(3,3)],[(6,3)],[(10,3)]]
  , check "f(o)o" "foo" [[(0,3),(1,1)]]
  , check ".*" "abcd" [[(0,4)], [(4,0)]]
  , check ".+" "abcd" [[(0,4)]]
  -- '.' doesn't seem to match nuls? a bit unfortunate since I wanted to see if
  -- trailing nuls were handled correctly.
  , check ".+" "\0abcd" [[(1,4)]]
  , check ".+" "abcd\0" [[(0,4)]]
  , check ".+" "foo\0abcd" [[(0,3)],[(4,4)]]
  , check ".+" "abcd\0foo" [[(0,4)],[(5,3)]]
  ]
