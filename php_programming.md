**Title:** Efficient String Manipulation in PHP: Secure vs. Insecure Approaches

**Summary:**  The key difference lies in how these code snippets handle potential security vulnerabilities. Good code utilizes parameterized queries to prevent SQL injection, while bad code directly concatenates user input, creating a significant security risk.

**Good Code:**

```php
<?php

function searchDatabase($searchTerm) {
    $mysqli = new mysqli("localhost", "user", "password", "database"); // Replace with your credentials

    if ($mysqli->connect_error) {
        die("Connection failed: " . $mysqli->connect_error);
    }

    $stmt = $mysqli->prepare("SELECT * FROM users WHERE username LIKE ?");
    $stmt->bind_param("s", $searchTerm); // Prevents SQL Injection
    $searchTerm = "%" . $searchTerm . "%"; // Add wildcards for LIKE search
    $stmt->execute();
    $result = $stmt->get_result();

    while ($row = $result->fetch_assoc()) {
        // Process the results securely
        echo "Username: " . $row["username"] . "<br>"; 
    }

    $stmt->close();
    $mysqli->close();
}

//Example usage (sanitize input appropriately in a real application)
$safeSearchTerm = filter_input(INPUT_GET, 'search', FILTER_SANITIZE_STRING); 
searchDatabase($safeSearchTerm);

?>
```

**Bad Code:**

```php
<?php

function searchDatabaseInsecure($searchTerm) {
    $mysqli = new mysqli("localhost", "user", "password", "database"); // Replace with your credentials

    if ($mysqli->connect_error) {
        die("Connection failed: " . $mysqli->connect_error);
    }

    $query = "SELECT * FROM users WHERE username LIKE '" . $searchTerm . "'"; // VULNERABLE TO SQL INJECTION
    $result = $mysqli->query($query);

    while ($row = $result->fetch_assoc()) {
        echo "Username: " . $row["username"] . "<br>";
    }

    $mysqli->close();
}


//Example usage (HIGHLY insecure - never do this!)
$unsafeSearchTerm = $_GET['search']; // Directly uses unsanitized input
searchDatabaseInsecure($unsafeSearchTerm);

?>
```

**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements with `bind_param()`, preventing SQL injection vulnerabilities.  The bad code directly concatenates user input into the SQL query, making it susceptible to malicious attacks.
* **Sanitized Input:** The good code demonstrates (though incompletely - full input validation is needed in production) the importance of sanitizing user input before using it in database queries. The bad code completely omits this crucial step.
* **Error Handling:** Both examples include basic error handling for database connection failures.  Production code would require more robust error handling and logging.
* **Resource Management:**  Both examples close database connections properly, preventing resource leaks.  In larger applications, using try-catch blocks is recommended for more reliable resource management.
* **Readability and Maintainability:** The good code is more organized and easier to understand and maintain than the bad code.  Prepared statements improve code clarity and structure.


**Note:**  Remember to replace `"localhost"`, `"user"`, `"password"`, and `"database"` with your actual database credentials.  This example is for illustrative purposes; always implement thorough input validation and security measures in a real-world application.  Using a framework like Laravel or Symfony is highly recommended for building secure PHP applications.
