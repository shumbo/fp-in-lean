def getNumA : IO Nat := do
  (<- IO.getStdout).putStrLn "A"
  pure 5

def getNumB : IO Nat := do
  (<- IO.getStdout).putStrLn "B"
  pure 7

def test : IO Unit := do
  let a : Nat := if (← getNumA) == 5 then 0 else (← getNumB)
  (← IO.getStdout).putStrLn s!"The answer is {a}"
