**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The key difference lies in how user input is handled and sanitized to prevent SQL injection and cross-site scripting (XSS) vulnerabilities.  Good code utilizes parameterized queries and escaping techniques, while bad code directly incorporates user input into SQL queries, creating significant security risks.


**Good Code:**

```php
<?php

// Database credentials (should be stored securely, not hardcoded!)
$db_host = "localhost";
$db_user = "your_username";
$db_pass = "your_password";
$db_name = "your_database";

// Establish database connection
$conn = new mysqli($db_host, $db_user, $db_pass, $db_name);
if ($conn->connect_error) {
  die("Connection failed: " . $conn->connect_error);
}

// Sanitize user inputs
$username = $_POST["username"] ?? '';
$password = $_POST["password"] ?? '';

$username = $conn->real_escape_string($username); // Escape string for SQL query
$password = password_hash($password, PASSWORD_DEFAULT); // Hash password securely

// Prepared statement to prevent SQL injection
$stmt = $conn->prepare("SELECT id FROM users WHERE username = ? AND password = ?");
$stmt->bind_param("ss", $username, $password);
$stmt->execute();
$stmt->bind_result($user_id);
$stmt->fetch();

if ($stmt->num_rows > 0) {
    session_start();
    $_SESSION["user_id"] = $user_id;
    header("Location: dashboard.php");
    exit();
} else {
    echo "Invalid username or password.";
}

$stmt->close();
$conn->close();

?>
```


**Bad Code:**

```php
<?php

// Database credentials (insecurely hardcoded)
$db_host = "localhost";
$db_user = "your_username";
$db_pass = "your_password";
$db_name = "your_database";

// Establish database connection
$conn = new mysqli($db_host, $db_user, $db_pass, $db_name);

// Unsecured user input directly embedded into query
$username = $_POST["username"];
$password = $_POST["password"];

$sql = "SELECT id FROM users WHERE username = '$username' AND password = '$password'";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
  // Proceed with session management (vulnerable)
  session_start();
  // ...
} else {
  echo "Invalid username or password";
}

$conn->close();

?>
```


**Key Takeaways:**

* **Prepared Statements:** The good code uses prepared statements, preventing SQL injection. This is crucial because it separates the SQL query structure from the user-supplied data, preventing malicious code from being executed.
* **Input Sanitization:** `mysqli_real_escape_string()` in the good code escapes special characters in user input, reducing the risk of SQL injection.  The bad code lacks this crucial step.
* **Password Hashing:** The good code uses `password_hash()` to securely store passwords, making them resistant to cracking.  The bad code stores passwords in plain text, highly vulnerable to attacks.
* **Error Handling:** While both examples include basic error handling for database connection, robust error handling should be more comprehensive, logging errors and preventing sensitive information exposure.
* **Secure Credentials:**  The good code emphasizes that database credentials *should not* be hardcoded directly in the script; this is a critical security vulnerability.  The bad code demonstrates this dangerous practice.
* **Session Management:**  While omitted for brevity, good session management is essential to prevent session hijacking, including using secure cookies and appropriate session timeouts.


