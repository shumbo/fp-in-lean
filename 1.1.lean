#eval 1 + 2
#eval 1 + 2 * 5

#eval String.append "Hello, " "Lean!"
#eval String.append "great " (String.append "oak " "tree")

#eval String.append "it is " (if 1 > 2 then "yes" else "no")

/-!
Exercise

- 42 + 19
  -> 61
- String.append "A" (String.append "B" "C")
  -> "ABC"
- String.append (String.append "A" "B") "C"
  -> "ABC"
- if 3 == 3 then 5 else 7
  -> 5
- if 3 == 4 then "equal" else "not equal"
  -> "not equal"
-/

#eval 42 + 19
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
#eval if 3 == 3 then 5 else 7
#eval if 3 == 4 then "equal" else "not equal"
