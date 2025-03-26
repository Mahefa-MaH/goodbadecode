'Good Code Example:  Calculates the sum of numbers in a range.
Sub GoodCodeExample()
  Dim sum As Double
  Dim i As Long
  sum = 0
  For i = 1 To 10
    sum = sum + i
  Next i
  MsgBox sum
End Sub

'Bad Code Example: Calculates the sum of numbers in a range, but uses inefficient and unclear methods.
Sub BadCodeExample()
  Dim a As Variant, b As Variant, c As Variant, d As Variant, e As Variant, f As Variant, g As Variant, h As Variant, i As Variant, j As Variant
  a = 1: b = 2: c = 3: d = 4: e = 5: f = 6: g = 7: h = 8: i = 9: j = 10
  MsgBox a + b + c + d + e + f + g + h + i + j
End Sub
