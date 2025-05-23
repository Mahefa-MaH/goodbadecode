**Title:** Secure PHP User Input Handling: A Comparison

**Summary:**  The key difference lies in how user input is sanitized and validated.  Good code uses parameterized queries and input filtering to prevent SQL injection and cross-site scripting (XSS) vulnerabilities, while bad code directly incorporates unsanitized user input into database queries and HTML output, leading to security risks.


**Good Code:**

```php
<?php

// Database connection details (replace with your actual credentials)
$db_host = "localhost";
$db_user = "your_username";
$db_pass = "your_password";
$db_name = "your_database";

try {
    $pdo = new PDO("mysql:host=$db_host;dbname=$db_name", $db_user, $db_pass);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    die("Database connection failed: " . $e->getMessage());
}

// Sanitize and validate user input
$username = filter_input(INPUT_POST, 'username', FILTER_SANITIZE_STRING);
$email = filter_input(INPUT_POST, 'email', FILTER_SANITIZE_EMAIL);


// Prepared statement to prevent SQL injection
$stmt = $pdo->prepare("INSERT INTO users (username, email) VALUES (?, ?)");
$stmt->execute([$username, $email]);

echo "User added successfully!";

} catch (PDOException $e) {
    echo "Error: " . $e->getMessage();
}

?>
```

**Bad Code:**

```php
<?php

// Vulnerable code - DO NOT USE
$username = $_POST['username'];
$email = $_POST['email'];

$query = "INSERT INTO users (username, email) VALUES ('$username', '$email')";
$result = mysql_query($query); // mysql_query is deprecated and insecure

if ($result) {
    echo "User added successfully!";
} else {
    echo "Error adding user: " . mysql_error();
}

?>
```


**Key Takeaways:**

* **Prepared Statements (Good Code):**  Prevent SQL injection by separating user input from the SQL query.  The database driver handles escaping special characters, eliminating the risk of malicious code execution.
* **Input Filtering (Good Code):**  `filter_input()` function sanitizes and validates user input according to specified filters, reducing the risk of XSS attacks.
* **Error Handling (Good Code):**  The `try-catch` block provides robust error handling, preventing unexpected crashes and revealing sensitive information.
* **PDO (Good Code):** Uses the PDO (PHP Data Objects) extension, which is more secure and provides better database abstraction than the deprecated `mysql_*` functions used in the bad code example.
* **Deprecation (Bad Code):** The `mysql_*` functions are deprecated and highly insecure.  Using them is a critical vulnerability.
* **SQL Injection (Bad Code):** Directly embedding unsanitized user input into SQL queries makes the application highly vulnerable to SQL injection attacks.
* **XSS Vulnerability (Bad Code):** The bad code is vulnerable to XSS if the username or email contains malicious JavaScript code.  This could allow attackers to steal session data or perform other malicious actions.
