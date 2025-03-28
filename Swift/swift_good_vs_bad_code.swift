// Good Code Example:  Using Structs for Data Modeling

struct User {
    let id: Int
    let name: String
    let email: String
}

func fetchUsers() -> [User] {
    // ... Fetch user data from a data source ...
    return [
        User(id: 1, name: "Alice", email: "alice@example.com"),
        User(id: 2, name: "Bob", email: "bob@example.com")
    ]
}


// Bad Code Example:  Using Dictionaries with inconsistent keys and types

let userData = [
    "id": 1,
    "name": "Charlie",
    "email": "charlie@example.com",
    "age": "30", // inconsistent type
    "address": ["street": "123 Main", "zip": "90210"] // nested dictionary
]


// Good Code Example:  Handling Errors with Result type

enum NetworkError: Error {
    case invalidData
    case networkFailure
}

func fetchData() async throws -> Data {
    // ...Network request...
    throw NetworkError.networkFailure
}

func processData() async {
    do {
        let data = try await fetchData()
        // ...process data...
    } catch {
        // ...handle error...
        print("Error: \(error)")
    }
}



// Bad Code Example: Ignoring Errors

func fetchDataIgnoringErrors() {
    // ...Network request that may throw an error...  No error handling is present
}


// Good Code Example: Using enums for representing states

enum LoadingState<T> {
    case loading
    case success(T)
    case failure(Error)
}

// Bad Code Example: Using bools for representing multiple states


var isLoading: Bool = false
var isSuccessful: Bool = false
var isFailed: Bool = false

