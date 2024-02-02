-- #eval "Hello!!!".dropRightWhile (· == '!')
-- #eval "Hello...   ".dropRightWhile (fun c => not (c.isAlphanum))

-- a function that performs the given action twice
def twice (action : IO Unit) : IO Unit := do
  action
  action

def nTimes (action : IO Unit) : Nat -> IO Unit
  | 0 => pure ()
  | n + 1 => do
    action
    nTimes action n

-- exercise
def main : IO Unit := do
  let englishGreeting := IO.println "Hello!"
  IO.println "Bonjour!"
  englishGreeting
