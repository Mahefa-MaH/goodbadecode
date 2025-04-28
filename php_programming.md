**Title:** Secure PHP User Input Handling: Safe vs. Risky

**Summary:**  The key difference lies in parameterized queries (prepared statements) for database interactions and proper input sanitization for preventing Cross-Site Scripting (XSS) and SQL injection vulnerabilities.  Failing to use these techniques exposes applications to severe security risks.


**Good Code:**

```php
<?php

// Database credentials (should be stored securely, not hardcoded!)
$db_host = 'localhost';
$db_user = 'your_username';
$db_pass = 'your_password';
$db_name = 'your_database';

// Create a database connection
$conn = new mysqli($db_host, $db_user, $db_pass, $db_name);

if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}


// Sanitize user input
$username = $conn->real_escape_string($_POST['username']);
$email = filter_var($_POST['email'], FILTER_SANITIZE_EMAIL);


// Prepare and execute a parameterized query (Prepared Statement)
$stmt = $conn->prepare("SELECT * FROM users WHERE username = ? AND email = ?");
$stmt->bind_param("ss", $username, $email);
$stmt->execute();
$result = $stmt->get_result();


if ($result->num_rows > 0) {
    echo "User found!";
} else {
    echo "User not found.";
}

$stmt->close();
$conn->close();

?>
```

**Bad Code:**

```php
<?php

// Vulnerable code - DO NOT USE
$db_host = 'localhost';
$db_user = 'your_username';
$db_pass = 'your_password';
$db_name = 'your_database';

$conn = new mysqli($db_host, $db_user, $db_pass, $db_name);

if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

$username = $_POST['username'];
$email = $_POST['email'];

$sql = "SELECT * FROM users WHERE username = '$username' AND email = '$email'";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    echo "User found!";
} else {
    echo "User not found.";
}

$conn->close();

?>
```

**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements, preventing SQL injection vulnerabilities by separating data from SQL code. The bad code directly concatenates user input into the SQL query, making it highly vulnerable.
* **XSS Prevention:** While not explicitly shown in the database query example, the good code demonstrates proper input sanitization using `mysqli_real_escape_string()` for the username and `filter_var()` for email, mitigating Cross-Site Scripting risks.  The bad code lacks any input sanitization, leaving it wide open to XSS attacks.
* **Security Best Practices:** The good code follows secure coding principles by using parameterized queries and input sanitization, protecting against common web application vulnerabilities. The bad code is directly susceptible to both SQL injection and XSS attacks.
* **Maintainability and Readability:** The good code is more organized and easier to understand, making it simpler to maintain and debug.  The bad code is less readable and more prone to errors.
* **Error Handling:** Both versions include basic error handling for the database connection.  Robust error handling should be expanded upon in a production environment.


Remember to replace placeholder database credentials with your actual values.  Always prioritize security when handling user input in PHP applications.
