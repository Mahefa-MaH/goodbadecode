**Title:** Secure PHP User Input Handling: A Comparison

**Summary:**  The good code example uses parameterized queries to prevent SQL injection vulnerabilities, while the bad code directly incorporates user input into the SQL query, creating a significant security risk.  This comparison highlights best practices for handling user input in PHP applications interacting with databases.

**Good Code:**

```php
<?php

// Secure way to handle user input with prepared statements
function getUserData($userId) {
    $pdo = new PDO('mysql:host=localhost;dbname=mydatabase', 'username', 'password');
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    $stmt = $pdo->prepare("SELECT * FROM users WHERE id = ?");
    $stmt->execute([$userId]);
    return $stmt->fetch(PDO::FETCH_ASSOC);
}


//Example usage (ensure userId is properly sanitized before passing to the function)
$userId = filter_input(INPUT_GET, 'id', FILTER_VALIDATE_INT); 
if ($userId !== false){
    $userData = getUserData($userId);
    if ($userData){
        echo "User ID: " . $userData['id'] . "<br>";
        echo "Username: " . $userData['username'] . "<br>";
    } else {
        echo "User not found.";
    }
} else {
    echo "Invalid User ID.";
}

?>
```


**Bad Code:**

```php
<?php

// Insecure way to handle user input - vulnerable to SQL injection
function getUserDataInsecure($userId) {
    $pdo = new PDO('mysql:host=localhost;dbname=mydatabase', 'username', 'password');
    $sql = "SELECT * FROM users WHERE id = " . $userId; // Directly concatenates user input into SQL
    $result = $pdo->query($sql);
    return $result->fetch(PDO::FETCH_ASSOC);
}


//Example usage - extremely vulnerable
$userId = $_GET['id']; // Directly taking user input without any validation or sanitization
$userData = getUserDataInsecure($userId);
if ($userData){
    echo "User ID: " . $userData['id'] . "<br>";
    echo "Username: " . $userData['username'] . "<br>";
} else {
    echo "User not found.";
}

?>
```

**Key Takeaways:**

* **Prepared Statements/Parameterized Queries:** The good code uses prepared statements (PDO::prepare()), which separate the SQL query structure from the user-supplied data. This prevents SQL injection attacks, where malicious users could inject harmful SQL code.
* **Input Validation and Sanitization:** The good code uses `filter_input()` to validate the input, ensuring that the `$userId` is an integer. This further reduces the risk of unexpected behavior and potential attacks. The bad code lacks any input validation making it vulnerable.
* **Error Handling:** Although both examples should have more robust error handling (e.g., try-catch blocks), the good code sets PDO error mode to exception, providing a more controlled way of handling database errors.
* **Security:** The bad code is directly vulnerable to SQL injection.  A malicious user could craft a `userId` value that injects arbitrary SQL commands, potentially allowing them to read, modify, or delete data from the database.
* **Maintainability:** The good code is more readable and easier to maintain due to its clear separation of concerns and the use of standard database interaction techniques.  The bad code is prone to errors and difficult to audit for security vulnerabilities.

