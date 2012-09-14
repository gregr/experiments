Sym = Nat
NS = Sym
Row ns = List [Symbol ns, Type]

inline {
zip #Nil _ = #Nil
zip _ #Nil = #Nil
zip (#Cons x xs) (#Cons y ys) = #Cons ##[x, y] (zip xs ys)

-- nats = [0..]
nats0 start = Cons start (nats0 (start + 1))
nats = nats0 0
}

Tuple types = Record (sigma Nat [zip nats types, Nothing])
Unit = Tuple Nil

variant Type {
  -- Type (implicitly present?)
  Symbol NS
  Int
  Nat
  Proc a b
  Sigma a (Proc a b)
  Record (Sigma NS (\ns -> [Row ns, Type]))
}

-- these should be special forms, not simple functions
--sigma (left : a) (right : a_to_b a) : Sigma a a_to_b
--record (row : Row ns) (dflt : a) : Record (sigma ns [Row ns, a])
--tuple items = record (zip nats items) undefined

variant Bool {
  False
  True
}

variant List a {
  Nil
  Cons a (List a)
}

-- how does this work?
-- variant Void {}
-- undefined : Void
