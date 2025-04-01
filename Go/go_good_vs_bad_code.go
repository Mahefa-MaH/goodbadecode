package main

import "fmt"

// Good Code Example:  Uses descriptive variable names, handles errors, and is well-structured.
func goodCode(a, b int) (int, error) {
	if b == 0 {
		return 0, fmt.Errorf("cannot divide by zero")
	}
	return a / b, nil
}

// Bad Code Example: Uses unclear variable names, lacks error handling, and is poorly structured.
func badCode(x, y int) int {
	return x / y // Potential for division by zero error, no error handling.
}

func main() {
	resultGood, err := goodCode(10, 2)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Good Code Result:", resultGood)
	}

	resultBad := badCode(10, 0) // This will panic if y is 0.
	fmt.Println("Bad Code Result:", resultBad)


}

