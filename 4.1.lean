inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos

-- def seven : Pos := 7
def seven : Pos :=
  Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ Pos.one)))))

-- def fourteen : Pos := seven + seven
-- def fortyNine : Pos := seven * seven

#check 5 + 3

class Plus (α : Type) where
  plus : α -> α -> α

instance : Plus Nat where
  plus := Nat.add

#eval Plus.plus 5 3

open Plus (plus)
#eval plus 6 2

def Pos.plus : Pos → Pos → Pos
  | Pos.one, k => Pos.succ k
  | Pos.succ n, k => Pos.succ (n.plus k)

instance : Plus Pos where
  plus := Pos.plus

def fourteen : Pos := plus seven seven

instance : Add Pos where
  add := Pos.plus

def fourteen' : Pos := seven + seven

def Pos.toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ n => n.toNat + 1

instance : ToString Pos where
  toString x := toString (x.toNat)

#eval s!"There are {seven}"

def Pos.mul : Pos → Pos → Pos
  | Pos.one, k => k
  | Pos.succ n, k => n.mul k + k

instance : Mul Pos where
  mul := Pos.mul


instance : Mul Pos where
  mul := Pos.mul

#eval [seven * Pos.one,
       seven * seven,
       Pos.succ Pos.one * seven]


-- α : type for which a natural number is overloaded
-- Nat : actual literal that was encountered in the program
-- class OfNat (α : Type) (_ : Nat) where
-- ofNat : α

inductive LT4 where
  | zero
  | one
  | two
  | three
deriving Repr

instance : OfNat LT4 0 where
  ofNat := LT4.zero

instance : OfNat LT4 1 where
  ofNat := LT4.one

instance : OfNat LT4 2 where
  ofNat := LT4.two

instance : OfNat LT4 3 where
  ofNat := LT4.three

#eval (3 : LT4)
--  #eval (4 : LT4)

instance : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat -> Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

-- ## exercises

-- ### Another representation

structure Pos' where
  succ ::
  pred : Nat

def Pos'.add (lhs : Pos') (rhs : Pos') : Pos' :=
  Pos'.succ (lhs.pred + rhs.pred + 1)

instance : Add Pos' where
  add := Pos'.add

def Pos'.mul (lhs : Pos') (rhs : Pos') : Pos' :=
  let a := lhs.pred
  let b := rhs.pred
  Pos'.succ (a * b + a + b)

instance : Mul Pos' where
  mul := Pos'.mul

def Pos'.toNat (p : Pos') := p.pred + 1

instance : ToString Pos' where
  toString x := toString (x.toNat)

instance : OfNat Pos' (n + 1) where
  ofNat := Pos'.succ n

-- tests
def three := Pos'.succ 2
def four := Pos'.succ 3
def five := Pos'.succ 4

#eval three + four
#eval three + five
#eval three * five
#eval three + four * five

#eval (20 : Pos')
-- #eval (0 : Pos')

-- ### Even numbers
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

-- tests
def six := Even.double 3
def eight := Even.double 4
def ten := Even.double 5
def twelve := Even.double 6

#eval six + eight
#eval six + ten + eight
#eval six * eight

-- ### HTTP Requests
-- TODO
