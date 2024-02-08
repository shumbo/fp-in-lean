-- this is a decidable proposition and can be used for `if`
#check 2 < 4

-- this one is not decidable
-- #check if (fun (x : Nat) => 1 + x) = (Nat.succ ·) then "yes" else "no"

#check (mixHash)
#eval mixHash 1 1
#eval mixHash 1 2

structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α

def NonEmptyList.toList {α : Type} (xs : NonEmptyList α) := xs.head :: xs.tail

instance [ToString α] : ToString (NonEmptyList α) where
  toString xs := toString xs.toList

instance [Hashable α] : Hashable (NonEmptyList α) where
  hash xs := mixHash (hash xs.head) (hash xs.tail)

inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α -> α -> BinTree α -> BinTree α

def BinTree.eq [BEq α] : BinTree α -> BinTree α -> Bool
  | BinTree.leaf, BinTree.leaf => true
  | BinTree.branch l x r, BinTree.branch l2 x2 r2 =>
    x == x2 && eq l l2 && eq r r2
  | _, _ => false

instance [BEq α] : BEq (BinTree α) where
  beq := BinTree.eq

def hashBinTree [Hashable α] : BinTree α → UInt64
  | BinTree.leaf =>
    0
  | BinTree.branch left x right =>
    mixHash 1 (mixHash (hashBinTree left) (mixHash (hash x) (hashBinTree right)))

instance [Hashable α] : Hashable (BinTree α) where
  hash := hashBinTree

instance : Append (NonEmptyList α) where
  append xs ys :=
    { head := xs.head, tail := xs.tail ++ ys.head :: ys.tail }

instance : HAppend (NonEmptyList α) (List α) (NonEmptyList α) where
  hAppend xs ys :=
    { head := xs.head, tail := xs.tail ++ ys }

#eval Functor.map (· + 5) [1, 2, 3]
#eval [1, 2, 3].map (· + 5)
#eval Functor.map toString (some (List.cons 5 List.nil))
#eval Functor.map List.reverse [[1,2,3], [4,5,6]]

#eval (· + 5) <$> [1,2,3]
#eval toString <$> (none : Option (List Nat))
#eval toString <$> (some [1,2,3])
#eval List.reverse <$> [[1,2,3], [4,5,6]]

instance : Functor NonEmptyList where
  map f xs := { head := f xs.head, tail := f <$> xs.tail }

-- ## exercise
instance : HAppend (List α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys := match xs with
  | [] => ys
  | x::xs => { head := x, tail := xs ++ (ys.head :: ys.tail) }

#eval [1, 2, 3] ++ NonEmptyList.mk 4 [5 , 6]
#eval ([] : List Nat) ++ NonEmptyList.mk 4 [5 , 6]
#eval ([] : List Nat) ++ NonEmptyList.mk 4 []

def BinTree.map {α β : Type} (f : α -> β) : BinTree α -> BinTree β
  | BinTree.leaf => BinTree.leaf
  | BinTree.branch left x right =>
    BinTree.branch (map f left) (f x) (map f right)

instance : Functor BinTree where
  map := BinTree.map
