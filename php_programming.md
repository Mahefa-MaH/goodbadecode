**Title:** Efficient String Manipulation in PHP: Secure vs. Insecure

**Summary:**  The key difference lies in using prepared statements to prevent SQL injection vulnerabilities in database interactions and employing built-in functions for safer string manipulation, avoiding potential exploits.  Poorly written code lacks these security measures and uses inefficient string concatenation.

**Good Code:**

```php
<?php

// Securely interacting with a database using prepared statements
$conn = new PDO("mysql:host=localhost;dbname=mydatabase", "user", "password");
$stmt = $conn->prepare("SELECT * FROM users WHERE username = ?");
$stmt->execute([$_POST['username']]);
$user = $stmt->fetch(PDO::FETCH_ASSOC);


// Efficient string manipulation using built-in functions
$firstName = "John";
$lastName = "Doe";
$fullName = sprintf("%s %s", $firstName, $lastName); // Safer and more efficient than concatenation

echo $fullName;


//Sanitizing user input using filter_var
$sanitizedEmail = filter_var($_POST['email'], FILTER_SANITIZE_EMAIL);
if (filter_var($sanitizedEmail, FILTER_VALIDATE_EMAIL)) {
    // Proceed with email processing
} else {
    // Handle invalid email
}

?>
```


**Bad Code:**

```php
<?php

// Vulnerable to SQL injection
$username = $_POST['username'];
$query = "SELECT * FROM users WHERE username = '$username'";
$result = mysql_query($query); // mysql_* functions are deprecated and insecure


// Inefficient string concatenation
$firstName = "John";
$lastName = "Doe";
$fullName = $firstName . " " . $lastName;  // Less efficient than sprintf


//No input sanitization
$email = $_POST['email'];
//Proceed with email processing, leaving it vulnerable to injection
?>
```

**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements, preventing SQL injection attacks by treating user input as data, not executable code. The bad code directly incorporates user input into the SQL query, making it extremely vulnerable.
* **Efficient String Manipulation:** `sprintf()` in the good code is generally faster and more readable than string concatenation in the bad code, especially for complex string formatting.
* **Deprecated Functions:** The good code avoids deprecated and insecure functions like `mysql_*`, replaced by the more secure and robust `PDO`.
* **Input Sanitization:** The good code demonstrates the use of `filter_var` for sanitizing user input, preventing Cross-Site Scripting (XSS) and other attacks.  The bad code lacks any input validation or sanitization, leaving the application wide open to various attacks.
* **Security Best Practices:** The good code adheres to modern security best practices, preventing common vulnerabilities, while the bad code showcases dangerous and outdated approaches.

