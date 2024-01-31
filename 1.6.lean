-- polymorphic point
structure PPoint (α : Type) where
  -- Greek letters for type arguments
  x : α
  y : α
deriving Repr

def natOrigin : PPoint Nat := { x := 0, y := 0 }

def replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α := { point with x := newX }
#check (replaceX)
#check (replaceX Nat)
#check replaceX Nat natOrigin 5
#eval replaceX Nat natOrigin 5
def replaceX' (point : PPoint α) (newX : α) : PPoint α := { point with x := newX }
#eval replaceX' natOrigin 5

inductive Sign where
  | pos
  | neg

-- computation over types
def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => 3
  | Sign.neg => -3

def primesUnder10 : List Nat := [2, 3, 5, 7]

inductive List' (α : Type) where
  | nil : List' α
  | cons (h : α) (t : List' α) : List' α
deriving Repr

def explicitPrimesUnder10 : List' Nat := List'.cons 2 (List'.cons 3 (List'.cons 5 (List'.cons 7 List'.nil)))

def length (α : Type) (xs : List α) :=
  match xs with
  | List.nil => 0
  | List.cons _ xs' => Nat.succ (length α xs')

def length' (α : Type) (xs : List α) :=
  match xs with
  | [] => 0
  | _::ys => Nat.succ (length' α ys)

-- use curly braces to declare implicit arguments
def replaceX'' {α : Type} (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

#eval replaceX'' natOrigin 100

def length'' {α : Type} (xs : List α) := match xs with
  | [] => 0
  | _::t => 1 + length'' t

def List'.length {α : Type} (xs : List' α) := match xs with
  | List'.nil => 0
  | List'.cons _ t => 1 + t.length

#eval explicitPrimesUnder10.length
#check List.length (α := Nat)

inductive Option' (α : Type) : Type where
  | none : Option' α
  | some (val : α) : Option' α

def List'.head? {α : Type} (xs : List' α) : Option' α := match xs with
  | List'.nil => Option'.none
  | List'.cons t _ => Option'.some t


#eval primesUnder10.head?

def fives : String × Int := { fst := "five", snd := 5 }
def fives' : String × Int := ("Five", 5)

def PetName : Type := String ⊕ String

def animals : List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

def howManyDogs (pets: List PetName) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: morePets => howManyDogs morePets + 1
  | Sum.inr _ :: morePets => howManyDogs morePets

#eval howManyDogs animals

-- inductive MyType : Type where
inductive MyType where
  | ctor : (α : Type) -> α -> MyType

-- inductive MyType' where
--   | ctor : (MyType' -> Int) -> MyType'

-- ### Exercises

-- Write a function to find the last entry in a list. It should return an Option.
def last? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => Option.none
  | h::t => match t with
    | [] => Option.some h
    | _ => last? t

#eval last? [1,2,3,4,5]
#eval last? ["a", "b", "c"]

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => Option.none
  | y::ys => if predicate y then y else ys.findFirst? predicate

#eval [1,2,3,4,5].findFirst? (λ x => x % 2 == 0)
#eval [1,2,3,4,5,14,32].findFirst? (λ x => x % 7 == 0)

def Prod.swap {α β : Type} (pair: α × β) : β × α := (pair.snd, pair.fst)
#eval (1, 5).swap
#eval ("hello", 1.4).swap

inductive Pet where
  | dog (name: String) : Pet
  | cat (name: String) : Pet
deriving Repr

def dog1 := Pet.dog "Shiro"
def cat1 := Pet.cat "Tama"

def pets : List Pet := [dog1, cat1]

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | x::xs', y::ys' => (x, y)::(zip xs' ys')
  | _, _ => []

#eval zip [1,2,3] ["one", "two", "three", "four"]
#eval zip [1,2,3, 4] ["one", "two"]

def take {α : Type} (n: Nat) (xs : List α) :=
  if n == 0 then [] else
  match xs with
  | y::ys => y::(take (n - 1) ys)
  | [] => []

#eval take 3 ["bolete", "oyster"]
#eval take 1 ["bolete", "oyster"]

def distribute {α β γ : Type} (p : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match p.snd with
  | Sum.inl b => Sum.inl (p.fst, b)
  | Sum.inr g => Sum.inr (p.fst, g)

def mul {α : Type} (p : Bool × α) : α ⊕ α :=
  match p.fst with
  | Bool.true => Sum.inl p.snd
  | Bool.false => Sum.inr p.snd
