// Good Code Example:  Using Kotlin's coroutines for network operations

import kotlinx.coroutines.*

fun fetchData(url: String): String {
    return withContext(Dispatchers.IO) {
        // Simulate network request
        delay(1000)
        "Data from $url"
    }
}

// Bad Code Example: Blocking the main thread for network operations


fun fetchDataBlocking(url: String): String {
    // Simulate network request - blocks main thread!
    Thread.sleep(1000)
    return "Data from $url"
}


//Good Code Example: Using View Binding for efficient view access


// Bad Code Example:  Finding views using findViewById repeatedly.

