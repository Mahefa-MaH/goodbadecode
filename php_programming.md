**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The primary difference lies in secure password hashing and input sanitization.  Good code employs parameterized queries and bcrypt hashing to prevent SQL injection and weak password vulnerabilities, while bad code is susceptible to these common attacks.

**Good Code:**

```php
<?php

// Database credentials (should be in a separate config file!)
$db_host = 'localhost';
$db_user = 'your_username';
$db_pass = 'your_password';
$db_name = 'your_database';

// Secure connection
$pdo = new PDO("mysql:host=$db_host;dbname=$db_name", $db_user, $db_pass);
$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

function registerUser($username, $password, $pdo) {
    // Sanitize inputs
    $username = htmlspecialchars($username, ENT_QUOTES, 'UTF-8'); 
    
    // Hash password using bcrypt
    $hashed_password = password_hash($password, PASSWORD_BCRYPT);

    // Prepared statement to prevent SQL injection
    $stmt = $pdo->prepare("INSERT INTO users (username, password) VALUES (?, ?)");
    $stmt->execute([$username, $hashed_password]);

    return true;
}

function loginUser($username, $password, $pdo) {
    $stmt = $pdo->prepare("SELECT password FROM users WHERE username = ?");
    $stmt->execute([$username]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user && password_verify($password, $user['password'])) {
        return true; 
    } else {
        return false;
    }
}


// Example usage (replace with actual form handling)
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $username = $_POST['username'];
    $password = $_POST['password'];
  
    if (registerUser($username, $password, $pdo)) {
        echo "User registered successfully!";
    } else {
        echo "Registration failed.";
    }
}

?>
```


**Bad Code:**

```php
<?php
$db_host = 'localhost';
$db_user = 'your_username';
$db_pass = 'your_password';
$db_name = 'your_database';

$conn = mysqli_connect($db_host, $db_user, $db_pass, $db_name);

if (!$conn) {
    die("Connection failed: " . mysqli_connect_error());
}


function registerUser($username, $password) {
    $password = md5($password); // Weak hashing!
    $sql = "INSERT INTO users (username, password) VALUES ('$username', '$password')"; // SQL Injection vulnerability
    if (mysqli_query($conn, $sql)) {
        return true;
    } else {
        return false;
    }
}

// ... similar insecure login function ...

?>
```


**Key Takeaways:**

* **Prepared Statements (Parameterized Queries):**  The good code uses prepared statements to prevent SQL injection vulnerabilities.  The bad code directly inserts user input into the SQL query, making it vulnerable to attacks.
* **Secure Password Hashing:** The good code uses `password_hash()` with bcrypt, a strong one-way hashing algorithm that protects against rainbow table attacks. The bad code uses `md5()`, which is easily cracked.
* **Input Sanitization:** The good code uses `htmlspecialchars()` to prevent cross-site scripting (XSS) attacks.  The bad code lacks any input sanitization.
* **Error Handling:** The good code uses PDO's exception handling for robust error management. The bad code's error handling is minimal.
* **Database Connection:** While not explicitly highlighted, using PDO in the good example generally offers advantages in terms of security and features over `mysqli`.  Best practice would be to use a more robust connection management solution.


This example demonstrates fundamental security principles. In a real-world application, additional measures like input validation, output encoding, and session management would be crucial. Remember to store your database credentials securely, preferably in a configuration file outside of your web root.
