module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption
-- Calculates the greatest common divisor of m and n
gcd :: Int -> Int -> Int
gcd m n
	| n == 0    = m
	| otherwise = gcd n (m `mod` n)

-- Calculates the number of prime numbers in the range 1 to n
phi :: Int -> Int
phi num
    = length [x | x <- [1..num], gcd num x == 1]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
-- Pre: a,b > 0
computeCoeffs a b
	| b == 0   = (1,0)
  |otherwise =  (v', u' - q * v')
  where
      ( q , r ) = quotRem a b
      ( u', v') = computeCoeffs b r

-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m
		| gcd a m == 1 = u `mod` m
    where
			(u,v) = computeCoeffs a m

-- Calculates (a^k mod m)
modPow :: Int -> Int -> Int -> Int
modPow a k m
		| k == 0    = 1 `mod` m
    | even k    = modPow a' j m
    | otherwise = (modPow a (k-1) m) * a `mod` m
    where
			 j  = (k `div` 2)
			 a' = ((a^2) `mod` m)

-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
--Pre: a /= 0
smallestCoPrimeOf a
    | a <= 2    = a + 1
    | otherwise = head [b | b <- [2..a], gcd a b == 1]
		-- used head instead of minimum to prevent redundant calculations



-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q
  = ((e,n), (d,n))
	where
		n = p * q
		e = smallestCoPrimeOf ((p-1)*(q-1))
		d = inverse e ((p-1)*(q-1))

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e,n)
  = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d,n)
  = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
-- Pre: The letters are in lower case
toInt letter
  = ord(letter) - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
--Pre: The letters are in lower case
toChar no
  = chr (no + ord 'a')

-- "adds" two letters
add :: Char -> Char -> Char
add ch1 ch2
  = toChar ((toInt ch1 + toInt ch2) `mod` noOfLetters)
	where noOfLetters = 26

-- "substracts" two letters
substract :: Char -> Char -> Char
substract ch1 ch2
  = toChar ((toInt ch1 - toInt ch2) `mod` noOfLetters)
	where noOfLetters = 26
-- used mod so that we get an answer even when the toChar val > 'z'

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
-- i represents the index
ecbEncrypt key message
	= [add key (message!!i)  | i <- [0..end]]
	where
		end = length message - 1
ecbDecrypt :: Char -> String -> String
ecbDecrypt key message
		= [substract (message!!i) key  | i <- [0..end]]
		where
			end = length message - 1

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string

--the names ci and c1 have been chosen from the spec sheet
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt _ _ []
	= []
cbcEncrypt key iv (c : cs)
	= c1 : ci
	where
		c1 = add key (add c iv)
		ci = cbcEncrypt key c1 cs
cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt _ _ []
  = []
cbcDecrypt key iv (c : cs)
	= c1 : ci
	where
		c1 = substract (substract c key) iv
		ci = cbcDecrypt key c cs
