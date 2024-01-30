inductive Bool' where
  | false : Bool'
  | true : Bool'
deriving Repr

#eval Bool'.false

inductive Nat' where
  | zero : Nat'
  | succ (n : Nat') : Nat'
deriving Repr

def isZero (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ _ => false

def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

#eval pred 100

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

def plus (n k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => plus (Nat.succ n) k'

#eval plus 1 3
#eval plus 2 10

def times (n k : Nat) : Nat :=
  match k with
  | Nat.zero => Nat.zero
  | Nat.succ k' => plus n (times n k')

#eval times 10 20

def minus (n k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => minus (pred n) k'

#eval minus 10 2
