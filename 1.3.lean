-- using `def` for definitions
def hello := "Hello"

-- specifying types
def lean : String := "Lean"

-- using definitions
#eval String.append hello (String.append " " lean)

-- defining functions can also be done with `def`
def add1 (n : Nat) : Nat := n + 1
#eval add1 7

-- function with multiple arguments
def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

#eval maximum 1 10
#eval maximum 22 3

-- checking function signature
#check maximum
-- checking function type with parentheses
#check (maximum)

-- ### Exercise

def joinStringsWith (a b c : String) : String :=
  String.append b (String.append a c)
#eval joinStringsWith ", " "one" "and another"

-- What is the type of joinStringsWith ": "? Check your answer with Lean.
--  String -> String -> String
#check (joinStringsWith ": ")

def volume (height width depth : Nat) : Nat :=
  height * width * depth

-- ## Defining Types

-- type alias
def Str : Type := String
def aStr : Str := "This is a string"

def NaturalNumber : Type := Nat
-- def thietyEight : NaturalNumber := (38)
def thietyEight : NaturalNumber := (38 : Nat)

abbrev N : Type := Nat
def thietyNine : N := 39

abbrev s : String := "hello"
