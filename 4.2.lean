#check (IO.println)
#check @IO.println

def List.sum {α : Type} [Add α] [OfNat α 0] : List α -> α
  | [] => 0
  | x::xs => x + xs.sum

def fourNats : List Nat := [1, 2, 3, 4]
#eval fourNats.sum

structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

instance [Add α] : Add (PPoint α) where
  add p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }

#eval (PPoint.mk 1 2) + (PPoint.mk 3 4)

-- ### Exercise

structure Even : Type where
  double ::
  half : Nat

def Even.add (a : Even) (b : Even) : Even :=
  Even.double (a.half + b.half)

instance : Add Even where
  add := Even.add

def Even.mul (a : Even) (b : Even) : Even :=
  Even.double (2 * a.half * b.half)

instance : Mul Even where
  mul := Even.mul

def Even.toNat (e : Even) : Nat :=
  e.half * 2

instance : ToString Even where
  toString x := toString (x.toNat)

instance : OfNat Even 0 where
  ofNat := Even.double 0

instance [OfNat Even n] : OfNat Even (n + 2) where
  ofNat := Even.double ((n + 2) / 2)

#eval (0 : Even)
-- #eval (3 : Even)
#eval (4 : Even)
#eval (6 : Even)
#eval (6 + 8 : Even )

#eval (254 : Even)
