import Primes

answer = sum $ takeWhile (<2000000) primes
