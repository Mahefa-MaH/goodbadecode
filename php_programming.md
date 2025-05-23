**Title:** Secure PHP User Authentication: Good vs. Bad Practices

**Summary:**  The key difference lies in how user input is handled and how sensitive data is protected. Good code utilizes prepared statements to prevent SQL injection and securely hashes passwords, while bad code is vulnerable to these common attacks.

**Good Code:**

```php
<?php
// Database credentials (should be stored securely, not in code!)
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


function authenticateUser($username, $password, $pdo) {
    $stmt = $pdo->prepare("SELECT * FROM users WHERE username = ?");
    $stmt->execute([$username]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user && password_verify($password, $user['password'])) {
        return $user; //Return user data (avoid sensitive data here)
    } else {
        return false;
    }
}


if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $username = $_POST["username"];
    $password = $_POST["password"];

    $user = authenticateUser($username, $password, $pdo);

    if ($user) {
        session_start();
        $_SESSION["user_id"] = $user['id']; //Store a user ID, not the entire user object
        header("Location: dashboard.php");
        exit();
    } else {
        echo "Invalid username or password.";
    }
}
?>

<form method="post">
    Username: <input type="text" name="username"><br>
    Password: <input type="password" name="password"><br>
    <input type="submit" value="Login">
</form>
```


**Bad Code:**

```php
<?php
$db_host = 'localhost';
$db_user = 'your_db_user';
$db_pass = 'your_db_password';
$db_name = 'your_db_name';

$conn = mysqli_connect($db_host, $db_user, $db_pass, $db_name);

if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $username = $_POST["username"];
    $password = $_POST["password"];

    $sql = "SELECT * FROM users WHERE username = '$username' AND password = '$password'";
    $result = mysqli_query($conn, $sql);

    if (mysqli_num_rows($result) > 0) {
        session_start();
        //Storing potentially sensitive data directly in session!
        $_SESSION["user"] = mysqli_fetch_assoc($result);
        header("Location: dashboard.php");
        exit();
    } else {
        echo "Invalid username or password.";
    }
}
?>
```


**Key Takeaways:**

* **Prepared Statements:** The good code uses prepared statements to prevent SQL injection vulnerabilities.  The bad code directly incorporates user input into the SQL query, making it highly vulnerable.
* **Password Security:** Good code uses `password_hash()` to securely store passwords and `password_verify()` to check them. The bad code stores passwords in plain text, a major security risk.
* **Error Handling:** Good code includes robust error handling (using `try...catch` with PDO). The bad code lacks proper error handling.
* **Session Management:**  Good code stores only necessary data (like a user ID) in the session, minimizing the impact of a potential session compromise. Bad code stores potentially sensitive user data directly into the session.
* **Data Sanitization/Validation:** While not explicitly shown, the good code implicitly benefits from the protection offered by prepared statements, which handle data escaping internally. The bad code is completely vulnerable to injection attacks if additional data is included in the SQL query (for example, a full name field).


This example demonstrates basic authentication.  In a real-world application, you should incorporate additional security measures, such as input validation, output encoding, and robust session management techniques.  Consider using a framework like Laravel or Symfony for more advanced and secure authentication implementations.
