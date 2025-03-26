/ Good K code:  Efficiently calculates the sum of a list.
sumList: {+/x}

/ Bad K code: Inefficiently calculates the sum of a list using recursion.
sumListRec:{[x] $[0=#[x];0;+/(first x),sumListRec rest x]}

/Example usage.
goodResult: sumList 1 2 3 4 5
badResult: sumListRec 1 2 3 4 5

/print the results.
goodResult;
badResult;
