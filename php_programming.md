**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The key difference lies in secure handling of user input and password hashing. Good code utilizes parameterized queries and strong password hashing (bcrypt), while bad code is vulnerable to SQL injection and uses insecure hashing algorithms.


**Good Code:**

```php
<?php

// Database credentials (should be stored securely, e.g., environment variables)
$host = 'localhost';
$dbname = 'mydatabase';
$user = 'myuser';
$password = 'mypassword';

try {
    $pdo = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    die("Database connection failed: " . $e->getMessage());
}

function authenticateUser($username, $password) {
    global $pdo;

    // Prepared statement to prevent SQL injection
    $stmt = $pdo->prepare("SELECT * FROM users WHERE username = ?");
    $stmt->execute([$username]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user && password_verify($password, $user['password'])) {
        return $user; // Return user data after successful authentication.  Consider using sessions.
    } else {
        return false;
    }
}


// Example usage:  Replace with actual user input handling
$username = 'testuser'; 
$password = 'securepassword';


$authenticatedUser = authenticateUser($username, $password);

if ($authenticatedUser) {
    echo "Authentication successful!";
    //Proceed with session management, etc.
} else {
    echo "Authentication failed.";
}

?>
```

**Bad Code:**

```php
<?php

$username = $_GET['username']; //Vulnerable to GET parameter manipulation
$password = $_GET['password']; //Vulnerable to GET parameter manipulation

$conn = mysql_connect('localhost', 'myuser', 'mypassword');
mysql_select_db('mydatabase', $conn);

$query = "SELECT * FROM users WHERE username = '$username' AND password = '$password'"; // SQL injection vulnerability
$result = mysql_query($query, $conn);

if (mysql_num_rows($result) > 0) {
  echo "Authentication successful!";
} else {
  echo "Authentication failed.";
}

mysql_close($conn);

?>
```


**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements with parameterized queries, preventing SQL injection attacks. The bad code directly concatenates user input into the SQL query, making it vulnerable.
* **Secure Password Hashing:** The good code employs `password_hash()` and `password_verify()` which uses a strong, adaptive hashing algorithm (bcrypt) for password storage and verification.  The bad code directly stores the password in plain text (or uses an outdated, easily crackable algorithm) making it highly insecure.
* **Error Handling:** The good code includes error handling (try-catch block) for database connections, providing more robust and reliable functionality. The bad code lacks error handling.
* **Deprecated Functions:** The bad code uses deprecated `mysql_*` functions.  The good code uses PDO which is the recommended approach for database interaction in PHP.
* **Input Sanitization:** While not explicitly shown in this simple example, the good code would ideally incorporate input validation and sanitization to further prevent attacks.  The bad code entirely lacks input validation.


This improved example provides a much more secure and robust authentication system.  Remember to always sanitize user inputs and implement proper session management for a complete and secure solution.
