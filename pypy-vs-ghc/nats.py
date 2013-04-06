def two(f):
  def two_nat(x): return f(f(x))
  return two_nat

def three(f):
  def three_nat(x): return f(f(f(x)))
  return three_nat

def add1(x): return x + 1

print three(three)(two)(add1)(0)

#> time python nats.py
#134217728

#real    0m32.921s
#user    0m32.842s
#sys     0m0.012s

#> time pypy nats.py
#134217728

#real    0m3.879s
#user    0m3.832s
#sys     0m0.032s

#> ghc --make nats.hs
#> time ./nats
#134217728

#real    0m3.591s
#user    0m3.568s
#sys     0m0.012s

#> ghc -O3 --make nats.hs
#> time ./nats
#134217728

#real    0m1.761s
#user    0m1.752s
#sys     0m0.004s
