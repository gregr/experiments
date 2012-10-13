type Sym
  Nat
type NS
  Sym
type Row ns
  List [(Symbol ns), Type]
type Fn a b
  Pi a (fn _ b)

variant Type
  Type
  Symbol NS
  Int
  Nat
  Pi a (Fn a b)
  Sigma a (Fn a b)
  Record (Sigma NS (fn ns [(Row ns), Type]))

variant Bool
  False
  True

variant List a
  Nil
  Cons a (List a)
