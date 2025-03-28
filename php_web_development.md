**Title:** Secure PHP User Registration: Good vs. Bad Practices

**Summary:**  The key difference lies in robust input sanitization and parameterized queries to prevent SQL injection in the good example, versus directly embedding user input in the bad example, leading to significant security vulnerabilities.

**Good Code:**

```php
<?php
  // Database credentials (should be in a secure config file, not hardcoded!)
  $host = 'localhost';
  $db   = 'mydatabase';
  $user = 'myusername';
  $pass = 'mypassword';

  // Sanitize user input
  $username = filter_input(INPUT_POST, 'username', FILTER_SANITIZE_STRING);
  $email    = filter_input(INPUT_POST, 'email', FILTER_SANITIZE_EMAIL);
  $password = password_hash($_POST['password'], PASSWORD_DEFAULT); // Hash the password!

  try {
    $pdo = new PDO("mysql:host=$host;dbname=$db", $user, $pass);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    // Prepared statement to prevent SQL injection
    $stmt = $pdo->prepare("INSERT INTO users (username, email, password) VALUES (?, ?, ?)");
    $stmt->execute([$username, $email, $password]);

    echo "Registration successful!";
  } catch (PDOException $e) {
    echo "Error: " . $e->getMessage();
  }
?>
```

**Bad Code:**

```php
<?php
  // Database credentials (hardcoded - very insecure!)
  $host = 'localhost';
  $db   = 'mydatabase';
  $user = 'myusername';
  $pass = 'mypassword';

  $username = $_POST['username'];
  $email    = $_POST['email'];
  $password = $_POST['password']; // Password not hashed - huge vulnerability!

  $sql = "INSERT INTO users (username, email, password) VALUES ('$username', '$email', '$password')";

  $conn = new mysqli($host, $user, $pass, $db);
  if ($conn->query($sql) === TRUE) {
    echo "Registration successful!";
  } else {
    echo "Error: " . $conn->error;
  }
  $conn->close();
?>
```


**Key Takeaways:**

* **Input Sanitization:** The good code uses `filter_input()` to sanitize user input, preventing cross-site scripting (XSS) attacks and other injection vulnerabilities. The bad code directly uses user input without any sanitization.
* **Prepared Statements/Parameterized Queries:** The good code utilizes prepared statements with PDO, effectively preventing SQL injection attacks. The bad code directly embeds user input into the SQL query, making it highly vulnerable.
* **Password Hashing:** The good code uses `password_hash()` to securely hash the password before storing it in the database.  The bad code stores the password in plain text, a major security risk.
* **Error Handling:** The good code uses a `try-catch` block to handle potential database errors gracefully. The bad code's error handling is rudimentary and potentially exposes sensitive information.
* **Database Credentials:** The good code *should* store database credentials in a secure configuration file outside the main code (not shown for brevity, but crucial). The bad code hardcodes credentials directly into the script, a significant security flaw.
* **Use of PDO:** PDO offers better security and database abstraction compared to the `mysqli` approach used in the bad code.


This example highlights critical security aspects often overlooked in PHP web development.  Always sanitize inputs, use parameterized queries, hash passwords, and handle errors properly.  Remember to store sensitive data securely outside of your main application code.
