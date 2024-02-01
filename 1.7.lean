-- automatic implicit arguments

def length (xs : List a) : Nat :=
  match xs with
  | [] => 0
  | _::ys => Nat.succ (length ys)

-- pattern-matching definitions
def length' : List α -> Nat
  | [] => 0
  | _::xs => 1 + (length' xs)

def drop : Nat -> List α -> List α
  | 0, xs => xs
  | _, [] => []
  | Nat.succ n, _ :: xs => drop n xs

def fromOption (default : α) : Option α -> α
  | none => default
  | some v => v

#eval (some "salmonberry").getD ""
#eval none.getD ""

def unzip_exp : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    (x :: (unzip_exp xys).fst, y :: (unzip_exp xys).snd)

def unzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped := unzip xys
    (x :: unzipped.fst, y :: unzipped.snd)

def unzip' : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let (xs, ys) := unzip xys
    (x :: xs, y::ys)

-- `let rec` for recursion
def reverse {α : Type} (xs : List α) : List α :=
  let rec aux : List α -> List α -> List α
    | [], soFar => soFar
    | y::ys, soFar => aux ys (y :: soFar)
  aux xs []

-- function expressions
#check fun x => x + 1
#check fun (x : Int) => x + 1
-- explicit arguments
#check fun {α : Type} (x : α) => x
-- pattern matching
#check fun
  | 0 => none
  | n + 1 => some n

def double : Nat -> Nat := fun
  | 0 => 0
  | Nat.succ n => double n + 2

#eval (· * 2) 5
#eval (2 + ·) 5


namespace NewNamespace
def triple (x : Nat) : Nat := 3 * x
def quadruple (x : Nat) : Nat := 4 * x
end NewNamespace

#check NewNamespace.triple

open NewNamespace in
#check triple


inductive Inline : Type where
  | lineBreak
  | string : String → Inline
  | emph : Inline → Inline
  | strong : Inline → Inline

def Inline.string? (inline : Inline) : Option String :=
  if let Inline.string s := inline then
    some s
  else none
