**Title:** Secure PHP User Input Handling: A Comparison

**Summary:**  The key difference lies in how user input is sanitized and validated. Good code uses parameterized queries and input filtering to prevent SQL injection and cross-site scripting (XSS), while bad code directly incorporates unsanitized user input into database queries, creating vulnerabilities.

**Good Code:**

```php
<?php

// Assuming a PDO database connection is already established: $pdo

function addUser($username, $email, $pdo) {
  $stmt = $pdo->prepare("INSERT INTO users (username, email) VALUES (?, ?)");
  $stmt->execute([$username, $email]); 

  //Input validation should happen BEFORE this point.  See Key Takeaways.
  //Error handling omitted for brevity, but crucial in production code.  
}


//Example of input sanitization and validation - crucial!
function sanitizeInput($input){
    //Basic example - needs improvement for production
    $input = htmlspecialchars($input, ENT_QUOTES, 'UTF-8'); //prevents XSS
    $input = trim($input); //removes whitespace
    return $input;
}

// Example usage with validation:
$username = sanitizeInput($_POST['username']);
$email = sanitizeInput($_POST['email']);

//Basic validation - add more robust checks
if(empty($username) || empty($email) || !filter_var($email, FILTER_VALIDATE_EMAIL)){
    die("Invalid Input");
}

addUser($username, $email, $pdo);


?>
```

**Bad Code:**

```php
<?php

// Assuming a mysql_connect database connection is already established - HIGHLY discouraged.
$username = $_POST['username'];
$email = $_POST['email'];

$query = "INSERT INTO users (username, email) VALUES ('$username', '$email')";
mysql_query($query); // Directly embedding user input - VULNERABLE!

?>
```

**Key Takeaways:**

* **Prepared Statements (Parameterized Queries):** The good code uses PDO's `prepare()` and `execute()` methods. This prevents SQL injection by separating the SQL query structure from the data, treating user input as data, not code.  The bad code directly concatenates user input into the SQL query, making it vulnerable to SQL injection attacks.
* **Input Sanitization & Validation:**  The good code demonstrates basic input sanitization using `htmlspecialchars()` to prevent XSS vulnerabilities and utilizes `filter_var()` for basic email validation.  Crucially, the code *validates* before using the input, checking if it's empty or improperly formatted.  The bad code entirely omits any input validation or sanitization, leaving it wide open to various attacks.
* **Security Best Practices:** The good code uses PDO, a more secure and robust database abstraction layer compared to the outdated `mysql_*` functions used in the bad code.  PDO also offers better error handling capabilities.  The bad code showcases deprecated functionality and is therefore vulnerable.
* **Error Handling:** While omitted for brevity, comprehensive error handling is vital in production systems.  The bad code lacks any error checking, potentially exposing sensitive information or causing unexpected behavior.  The good code (while not explicitly showing error handling) implies that proper error handling would be implemented.
* **Database Abstraction Layer:** Using PDO or similar database abstraction layers (mysqli) is essential for secure and maintainable code.  Direct interaction with database functions (like `mysql_query`) is highly discouraged due to increased vulnerability and maintenance difficulties.


