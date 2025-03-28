      ⍝ Good APL code: concise and readable
      grades ← 100 90 80 70 60
      average ← (+/grades) ÷ ⍴grades
      average

      ⍝ Bad APL code: unclear and inefficient
      grades1 ← 100
      grades2 ← 90
      grades3 ← 80
      grades4 ← 70
      grades5 ← 60
      sum ← grades1 + grades2 + grades3 + grades4 + grades5
      count ← 5
      average1 ← sum ÷ count
      average1

