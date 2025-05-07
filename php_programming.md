**Title:** Secure PHP User Input Handling: A Comparison

**Summary:**  The key difference lies in how user input is sanitized and validated.  Good code uses parameterized queries and input filtering to prevent SQL injection and cross-site scripting (XSS) vulnerabilities, while bad code directly incorporates user input into queries, leaving it vulnerable.

**Good Code:**

```php
<?php

//  Database connection details (replace with your actual credentials)
$host = 'localhost';
$dbname = 'mydatabase';
$user = 'myuser';
$password = 'mypassword';

try {
    $pdo = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);


    // Sanitize and validate user input (example: searching for users)
    $username = filter_input(INPUT_GET, 'username', FILTER_SANITIZE_STRING);

    // Prepared statement to prevent SQL injection
    $stmt = $pdo->prepare("SELECT * FROM users WHERE username LIKE ?");
    $stmt->execute(["%$username%"]); //Use parameterized query


    $users = $stmt->fetchAll(PDO::FETCH_ASSOC);

    //Output results securely (e.g., using escaping or templating engine)
    foreach($users as $user){
        echo htmlspecialchars($user['username'])."<br>"; //Escaping output 
    }


} catch (PDOException $e) {
    die("Database error: " . $e->getMessage());
}

?>
```

**Bad Code:**

```php
<?php
// Database connection details (replace with your actual credentials)
$host = 'localhost';
$dbname = 'mydatabase';
$user = 'myuser';
$password = 'mypassword';

$username = $_GET['username']; //Directly using user input without sanitization

$conn = new mysqli($host, $user, $password, $dbname);
if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

//Vulnerable to SQL injection!
$sql = "SELECT * FROM users WHERE username LIKE '%" . $username . "%'"; 
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    while($row = $result->fetch_assoc()) {
        echo $row["username"]."<br>"; //No output escaping
    }
} else {
    echo "0 results";
}
$conn->close();
?>
```


**Key Takeaways:**

* **Prepared Statements/Parameterized Queries:**  The good code uses prepared statements, preventing SQL injection by separating data from SQL code. The bad code directly concatenates user input into the SQL query, making it vulnerable.
* **Input Validation and Sanitization:** The good code uses `filter_input()` to sanitize user input, removing potentially harmful characters. The bad code directly takes input from `$_GET`, leaving it susceptible to XSS and SQL injection attacks.
* **Output Encoding:** The good code uses `htmlspecialchars()` to escape output, preventing XSS vulnerabilities.  The bad code directly echoes user-provided data, which can lead to XSS.
* **Error Handling:** The good code uses a `try-catch` block to handle potential database errors gracefully. The bad code has less robust error handling.
* **Database Connection:** While both examples show a database connection, best practices include using a more secure connection method (like PDO) and managing credentials appropriately.  Using a configuration file to store database credentials is much more secure than hardcoding them in the script.


