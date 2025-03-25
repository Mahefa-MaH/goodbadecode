**Title:** Efficient Go Concurrency: Goroutines vs. Channels

**Summary:** Goroutines provide lightweight concurrency through independent functions, while channels offer synchronized communication and data sharing between them, preventing race conditions and ensuring data integrity.  Improper channel usage can lead to deadlocks and performance bottlenecks.

**Good Code:**

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Second) // Simulate work
		results <- j * 2
	}
}

func main() {
	jobs := make(chan int, 100) // Buffered channel to prevent blocking
	results := make(chan int, 100)
	var wg sync.WaitGroup

	for w := 1; w <= 3; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs) // Signal no more jobs

	wg.Wait() // Wait for all workers to finish
	close(results)

	for r := range results {
		fmt.Printf("Result: %d\n", r)
	}
}

```

**Bad Code:**

```go
package main

import (
	"fmt"
	"time"
)

func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Second) // Simulate work
		results <- j * 2
	}
}

func main() {
	jobs := make(chan int)
	results := make(chan int)

	go worker(1, jobs, results)
	go worker(2, jobs, results)

	jobs <- 1
	jobs <- 2
	close(jobs) //This will cause the program to exit before the goroutines are finished

	fmt.Println("Sent jobs") // This line will execute before the workers complete.
}
```

**Key Takeaways:**

* **Explicit Wait Group:** The good code uses a `sync.WaitGroup` to ensure all goroutines complete before the main function exits, preventing data loss or unexpected behavior.  The bad code lacks this crucial synchronization.
* **Buffered Channels:** The good code employs buffered channels to handle potential blocking scenarios, improving efficiency.  The bad code uses unbuffered channels which can lead to deadlocks if the sender is faster than the receiver.
* **Channel Closure:** The good code correctly closes the `jobs` channel to signal the end of the job queue, allowing the workers to gracefully exit.  The bad code's `close(jobs)` is prematurely called, resulting in incomplete work.
* **Error Handling:**  While not explicitly shown, the good code implicitly handles potential errors by using buffered channels and the `WaitGroup`.  Adding explicit error checks would make it even more robust. The bad code lacks any error handling mechanisms.
* **Resource Management:**  The `defer wg.Done()` in the good code ensures proper cleanup, even if a `panic` occurs.  The bad code doesn't have a mechanism to ensure proper resource cleanup.

The good code demonstrates best practices for Go concurrency, leading to more robust, efficient, and predictable programs.  The bad code highlights common pitfalls that can lead to subtle bugs, race conditions, deadlocks, and performance issues.
