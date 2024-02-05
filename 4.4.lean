-- ## Arrays

-- Array α
def northernTrees : Array String :=
  #["sloe", "birch", "elm", "oak"]

#eval northernTrees[2]

-- ## Non-Empty Lists
structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α

def idahoSpiders : NonEmptyList String := {
  head := "Banded Garden Spider",
  tail := [
    "Long-legged Sac Spider",
    "Wolf Spider",
    "Hobo Spider",
    "Cat-faced Spider"
  ]
}

def NonEmptyList.get? {α : Type} : NonEmptyList α -> Nat -> Option α
  | xs, 0 => some xs.head
  | {head := _, tail := []}, _ + 1 => none
  | {head := _, tail := h :: t}, n + 1 => get? {head := h, tail := t} n

def NonEmptyList.get?' : NonEmptyList α → Nat → Option α
  | xs, 0 => some xs.head
  | xs, n + 1 => xs.tail.get? n

abbrev NonEmptyList.inBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

theorem atLeastThreeSpiders : idahoSpiders.inBounds 2 := by simp

theorem notSixSpiders : ¬idahoSpiders.inBounds 5 := by simp

def NonEmptyList.get {α : Type} (xs : NonEmptyList α) (i : Nat) (_ : xs.inBounds i) : α :=
  match i with
  | 0 => xs.head
  | n + 1 => xs.tail[n]

#eval idahoSpiders.get 2 atLeastThreeSpiders

-- ## Overloading indexing

instance : GetElem (NonEmptyList α) Nat α NonEmptyList.inBounds where
  getElem := NonEmptyList.get
