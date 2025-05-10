**Title:** Secure PHP User Input Handling: A Comparison

**Summary:**  The key difference lies in how user input is sanitized and validated. Good code uses parameterized queries to prevent SQL injection and filters input to prevent cross-site scripting (XSS), while bad code is vulnerable to both.

**Good Code:**

```php
<?php
    $db = new PDO('mysql:host=localhost;dbname=mydatabase', 'username', 'password');
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    if ($_SERVER["REQUEST_METHOD"] == "POST") {
        $username = filter_input(INPUT_POST, 'username', FILTER_SANITIZE_STRING);
        $password = password_hash(filter_input(INPUT_POST, 'password', FILTER_SANITIZE_STRING), PASSWORD_DEFAULT); //Hashing for security

        $stmt = $db->prepare("INSERT INTO users (username, password) VALUES (?, ?)");
        $stmt->execute([$username, $password]);
        echo "User registered successfully!";
    } else {
        echo "<form method='post'>
                Username: <input type='text' name='username'><br>
                Password: <input type='password' name='password'><br>
                <input type='submit' value='Register'>
              </form>";
    }
?>
```

**Bad Code:**

```php
<?php
    $db = mysql_connect('localhost', 'username', 'password');
    mysql_select_db('mydatabase');

    if ($_SERVER["REQUEST_METHOD"] == "POST") {
        $username = $_POST['username'];
        $password = $_POST['password']; //No hashing or sanitization

        $query = "INSERT INTO users (username, password) VALUES ('$username', '$password')";
        mysql_query($query);
        echo "User registered successfully!";
    }
    // ...rest of the code (form etc.) remains the same as Good Code
?>
```

**Key Takeaways:**

* **Prepared Statements (PDO):** The good code uses PDO prepared statements, preventing SQL injection vulnerabilities by separating SQL code from user-supplied data.  The bad code directly concatenates user input into the SQL query, making it extremely vulnerable.

* **Input Sanitization (filter_input):**  `filter_input` in the good code sanitizes user input, reducing the risk of XSS attacks.  The bad code doesn't sanitize or validate any input, leaving it open to various attacks.

* **Password Hashing:** The good code uses `password_hash` to securely store passwords. The bad code stores passwords in plain text, making them easily compromised.

* **Error Handling:** While not explicitly shown, the good code uses `PDO::ERRMODE_EXCEPTION` for better error handling and security. The bad code lacks proper error handling.

* **MySQLi vs. PDO:** The good code uses PDO (PHP Data Objects), a more modern and flexible database interface that supports prepared statements.  The bad code uses the outdated `mysql_*` functions, which are deprecated and insecure.  Consider using `mysqli` as a better, but still less robust alternative to PDO.
