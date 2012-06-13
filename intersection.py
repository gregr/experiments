def one(s):
  def onez(z):
    return s(z)
  return onez

def two(s):
  def twoz(z):
    return s(s(z))
  return twoz

def three(s):
  def threez(z):
    return s(s(s(z)))
  return threez

def add1(n):
  return n + 1

