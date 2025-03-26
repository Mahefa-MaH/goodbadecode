// Good Code Example: Using Kotlin's coroutines for network operations

import kotlinx.coroutines.*

fun fetchDataFromNetwork(): String {
    return runBlocking {
        withContext(Dispatchers.IO) {
            // Simulate network request
            delay(1000)
            "Data from network"
        }
    }
}


// Bad Code Example: Blocking the main thread for network operations

fun fetchDataFromNetworkBad(): String {
    // Simulate network request - This blocks the main thread!
    Thread.sleep(1000) 
    return "Data from network"
}

// Good Code Example: Using View Binding for efficient view access

// In your activity layout XML file (activity_main.xml) add a TextView with id: myTextView
// Then you can use the ViewBinding
// In your activity:

//private lateinit var binding: ActivityMainBinding

//In onCreate:

//binding = ActivityMainBinding.inflate(layoutInflater)
//setContentView(binding.root)
//binding.myTextView.text = "Hello from ViewBinding"

// Bad Code Example: Using findViewById for view access - less efficient and prone to errors


//In onCreate:
//val myTextView = findViewById<TextView>(R.id.myTextView)
//myTextView.text = "Hello from findViewById"
