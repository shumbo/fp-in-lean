def woodlandCritters : List String :=
  ["hedgehog", "deer", "snail"]

def hedgehog := woodlandCritters[0]
def deer := woodlandCritters[1]
def snail := woodlandCritters[2]

-- def oops := woodlandCritters[3]

def onePlusOneIsTwo : 1 + 1 = 2 := rfl
-- def onePlusOneIsFifteen : 1 + 1 = 15 := rfl

theorem onePlusOneIsTwo' : 1 + 1 = 2 := by
  simp

theorem addAndAppend : 1 + 1 = 2 ∧ "Str".append "ing" = "String" :=
  And.intro rfl rfl

theorem andImpliesOr : A ∧ B → A ∨ B :=
  fun andEvidence =>
    match andEvidence with
    | And.intro a _ => Or.inl a

-- def third (xs : List α) : α := xs[2]
def third (xs : List α) (ok : xs.length > 2) : α := xs[2]
#eval third woodlandCritters (by simp)

theorem ex1 : 2 + 3 = 5 := rfl
theorem ex2 : "Hello, ".append "world" = "Hello, world" := rfl
theorem ex3 : 5 < 18 := by simp

theorem ex1' : 2 + 3 = 5 := by simp
theorem ex2' : "Hello, ".append "world" = "Hello, world" := by simp
theorem ex3' : 5 < 18 := by simp

def fifth {α : Type} (xs : List α) (ok : xs.length >= 5) : α := xs[4]
