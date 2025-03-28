package main

import (
	"fmt"
	"runtime"
	"sync"
)

// Good example: Using goroutines and channels for concurrency
func goodConcurrency(n int) []int {
	ch := make(chan int, n)
	var wg sync.WaitGroup
	for i := 0; i < n; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			ch <- i * 2
		}(i)
	}
	wg.Wait()
	close(ch)
	result := make([]int, n)
	for i := 0; i < n; i++ {
		result[i] = <-ch
	}
	return result
}

// Bad example: Excessive goroutines without proper synchronization
func badConcurrency(n int) []int {
	result := make([]int, n)
	for i := 0; i < n; i++ {
		go func(i int) {
			result[i] = i * 2 // Race condition!
		}(i)
	}
	runtime.Gosched() //This won't prevent the race condition.
	return result
}

func main() {
	fmt.Println("Good Concurrency:", goodConcurrency(5))
	fmt.Println("Bad Concurrency:", badConcurrency(5)) //Likely to produce incorrect results due to race condition.

}
