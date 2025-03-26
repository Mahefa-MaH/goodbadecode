// Good Code Example: Using Kotlin Coroutines for Network Calls

import kotlinx.coroutines.*

fun makeNetworkCall(): String {
    return runBlocking {
        withContext(Dispatchers.IO) {
            // Simulate network call
            delay(1000)
            "Network call successful"
        }
    }
}


// Bad Code Example: Blocking the Main Thread for Network Calls

fun makeNetworkCallBad(): String {
    // Simulate network call - This blocks the main thread!
    Thread.sleep(1000)
    return "Network call successful (but blocking the main thread!)"
}

//Good Code Example: Using Data Classes for structured data

data class User(val id:Int, val name:String, val email:String)

//Bad Code Example:  Not using data classes

class UserBad(val id:Int, val name:String, val email:String){
    fun getId():Int{return id}
    fun getName():String{return name}
    fun getEmail():String{return email}

}
