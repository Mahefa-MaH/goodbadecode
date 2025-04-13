**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The key difference lies in preventing SQL injection vulnerabilities and using prepared statements for secure database interaction in the "good" code, while the "bad" code directly incorporates user input into the query, making it highly vulnerable.

**Good Code:**

```php
<?php

// Database credentials (should be stored securely, e.g., environment variables)
$host = "localhost";
$dbname = "mydatabase";
$user = "myuser";
$password = "mypassword";

try {
    $dbh = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
    $dbh->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION); 

    // Sanitize username (optional but recommended)
    $username = filter_input(INPUT_POST, 'username', FILTER_SANITIZE_STRING);

    // Prepared statement to prevent SQL injection
    $stmt = $dbh->prepare("SELECT * FROM users WHERE username = ?");
    $stmt->execute([$username]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user && password_verify($_POST['password'], $user['password'])) {
        // Successful login - set session variables etc.
        session_start();
        $_SESSION['user_id'] = $user['id'];
        header("Location: dashboard.php");
        exit;
    } else {
        echo "Incorrect username or password.";
    }
} catch (PDOException $e) {
    echo "Error: " . $e->getMessage();
}

?>
```


**Bad Code:**

```php
<?php
// Database credentials (insecure placement!)
$host = "localhost";
$dbname = "mydatabase";
$user = "myuser";
$password = "mypassword";


$username = $_POST['username'];
$password = $_POST['password'];

$conn = mysqli_connect($host, $user, $password, $dbname);

if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

// **SQL INJECTION VULNERABILITY HERE**
$sql = "SELECT * FROM users WHERE username = '$username' AND password = '$password'";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    // Successful login (no session handling - insecure)
    echo "Login successful!";
} else {
    echo "Incorrect username or password.";
}

$conn->close();

?>
```

**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements, preventing SQL injection attacks by separating data from SQL code. The bad code directly concatenates user input into the SQL query, leaving it vulnerable to injection.
* **Secure Password Handling:** While both examples are simplistic, the good code uses `password_verify()` for secure password comparison (assuming passwords are properly hashed during registration).  The bad code stores and compares passwords directly, which is extremely insecure.
* **Error Handling:** The good code employs a `try-catch` block for robust error handling and prevents revealing sensitive information in error messages.
* **Input Sanitization:** The good code uses `filter_input()` for basic input sanitization; however, this is not a replacement for prepared statements.  The bad code lacks any input sanitization.
* **Session Management:** The good example demonstrates secure session management which is missing in the bad example.
* **Secure Credential Handling:**  The good code implies secure storage of database credentials (environment variables), while the bad example directly exposes them in the code.


This example highlights only one aspect of secure PHP programming.  More robust authentication mechanisms, input validation, and output encoding are crucial for building secure applications.
