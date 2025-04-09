**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The key difference lies in how user input is handled and how sensitive data is protected.  Good code utilizes parameterized queries and password hashing to prevent SQL injection and insecure password storage, while bad code exposes vulnerabilities.

**Good Code:**

```php
<?php

// Database credentials (should be stored securely, not hardcoded!)
$db_host = 'localhost';
$db_user = 'your_db_user';
$db_pass = 'your_db_password';
$db_name = 'your_db_name';

try {
    $pdo = new PDO("mysql:host=$db_host;dbname=$db_name", $db_user, $db_pass);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    die("Database connection failed: " . $e->getMessage());
}

function authenticateUser($username, $password) {
    global $pdo;

    // Use prepared statements to prevent SQL injection
    $stmt = $pdo->prepare("SELECT * FROM users WHERE username = ?");
    $stmt->execute([$username]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user && password_verify($password, $user['password'])) {
        // Successful authentication -  Do not store the password in the session!  Use a secure session ID.
        session_start();
        $_SESSION['user_id'] = $user['id']; //Or a unique session ID
        return true;
    }
    return false;
}

// Example usage:
if (isset($_POST['username'], $_POST['password'])) {
    $username = $_POST['username'];
    $password = $_POST['password'];

    if (authenticateUser($username, $password)) {
        echo "Authentication successful!";
    } else {
        echo "Authentication failed.";
    }
}
?>

```


**Bad Code:**

```php
<?php
$username = $_POST['username'];
$password = $_POST['password'];

$db_host = 'localhost';
$db_user = 'your_db_user';
$db_pass = 'your_db_password';
$db_name = 'your_db_name';

$conn = mysqli_connect($db_host, $db_user, $db_pass, $db_name);

if (!$conn) {
    die("Connection failed: " . mysqli_connect_error());
}

// **Vulnerable to SQL injection!**
$sql = "SELECT * FROM users WHERE username = '$username' AND password = '$password'";
$result = mysqli_query($conn, $sql);

if (mysqli_num_rows($result) > 0) {
    echo "Login successful!";
    session_start();
    $_SESSION['username'] = $username; // Storing username directly in session - bad practice.
} else {
    echo "Login failed!";
}

mysqli_close($conn);
?>
```

**Key Takeaways:**

* **Prepared Statements/Parameterized Queries:** The good code uses prepared statements to prevent SQL injection vulnerabilities.  The bad code directly concatenates user input into the SQL query, making it vulnerable.
* **Password Hashing:** The good code utilizes `password_verify()` which is crucial for secure password storage.  The bad code stores passwords in plain text, a major security risk.
* **Error Handling:** The good code includes comprehensive error handling (using try-catch blocks and PDO error modes). The bad code has limited error handling.
* **Session Management:** The good code avoids storing sensitive data like passwords directly in the session. The bad code stores the username directly in the session, a potential security risk.
* **Database Connection:** Good code uses PDO which provides better security features and handles database errors better than the older mysqli method.
* **Security Best Practices:** Good code adheres to basic security principles, while bad code is vulnerable to common attacks.


