**Title:** Secure PHP User Input Handling: Safe vs. Unsafe

**Summary:**  The key difference lies in how user input is sanitized and validated.  Good code uses parameterized queries or prepared statements to prevent SQL injection and escapes HTML to prevent XSS attacks, while bad code directly incorporates unsanitized user input, creating significant security vulnerabilities.


**Good Code:**

```php
<?php

// Secure user input handling with prepared statements
function getUserData($userId, $pdo) {
    $stmt = $pdo->prepare("SELECT * FROM users WHERE id = ?");
    $stmt->execute([$userId]);
    return $stmt->fetch(PDO::FETCH_ASSOC);
}


// Secure output escaping with htmlspecialchars
function displayUserData($userData) {
    if ($userData) {
        echo "<p>ID: " . htmlspecialchars($userData['id']) . "</p>";
        echo "<p>Username: " . htmlspecialchars($userData['username']) . "</p>";
    } else {
        echo "<p>User not found.</p>";
    }
}


// Database connection (replace with your credentials)
$pdo = new PDO('mysql:host=localhost;dbname=your_database', 'your_user', 'your_password');
$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);


// Example usage (always sanitize user input before using it)
$userId = filter_input(INPUT_GET, 'id', FILTER_SANITIZE_NUMBER_INT);
$userData = getUserData($userId, $pdo);
displayUserData($userData);


?>
```

**Bad Code:**

```php
<?php

// Insecure user input handling - vulnerable to SQL injection and XSS
$userId = $_GET['id'];
$username = $_GET['username'];

$query = "SELECT * FROM users WHERE id = '$userId'";
$result = mysql_query($query); //Deprecated function - using for demonstration of bad practice

if ($result) {
    $row = mysql_fetch_assoc($result);
    echo "<p>ID: " . $row['id'] . "</p>";
    echo "<p>Username: " . $row['username'] . "</p>";
} else {
    echo "<p>User not found.</p>";
}
?>
```


**Key Takeaways:**

* **Prepared Statements/Parameterized Queries:** The good code uses prepared statements to prevent SQL injection. This separates the SQL query from the user-supplied data, neutralizing malicious input.
* **Input Sanitization:**  `filter_input()` in the good code helps sanitize user input, reducing the risk of unexpected data types or malicious code.
* **Output Escaping:** `htmlspecialchars()` in the good code escapes HTML special characters, preventing Cross-Site Scripting (XSS) attacks.  The bad code directly echoes user input, making it vulnerable to XSS.
* **Error Handling:** The good code uses a try-catch block for better error handling and security. The bad code has minimal error handling.
* **Deprecated Functions:** The bad code uses the deprecated `mysql_*` functions, which are insecure and no longer supported.  The good code uses PDO, a more secure and robust database abstraction layer.
* **Data Validation:**  The good code implicitly validates the `id` as an integer via `FILTER_SANITIZE_NUMBER_INT`.  The bad code performs no input validation.


The good code demonstrates best practices for secure PHP development, mitigating common vulnerabilities. The bad code exemplifies the risks of insecure coding practices.  Always prioritize security when handling user input in your PHP applications.
