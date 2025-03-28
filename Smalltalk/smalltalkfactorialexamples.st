Transcript show: 'Good Code Example:'.
Transcript cr.
Transcript show: 'Calculating the factorial of 5: ', 5 factorial printString.
Transcript cr.


Transcript show: 'Bad Code Example:'.
Transcript cr.
a := 5.
b := 1.
[a > 0] whileTrue: [ b := b * a. a := a - 1].
Transcript show: 'Calculating the factorial of 5 (badly): ', b printString.
Transcript cr.

