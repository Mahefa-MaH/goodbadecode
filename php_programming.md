**Title:** Secure PHP User Input Handling: Prevention vs. Sanitization

**Summary:**  Secure PHP input handling prioritizes prevention by validating input *before* processing, unlike sanitization, which attempts to clean potentially harmful data *after* it's received. Prevention significantly reduces the attack surface and is far more reliable.

**Good Code:**

```php
<?php

function registerUser(array $userData): bool {
    // Validation: Check if all required fields exist and meet criteria.
    $requiredFields = ['username', 'email', 'password'];
    foreach ($requiredFields as $field) {
        if (!isset($userData[$field]) || empty($userData[$field])) {
            return false; // Fail early if required data is missing
        }
    }

    // Input validation:  More specific checks.
    if (!filter_var($userData['email'], FILTER_VALIDATE_EMAIL)) {
        return false;
    }

    if (strlen($userData['password']) < 8) {
        return false;
    }


    //Escaping ONLY for database interaction (Prepared Statements)
    //Sanitize ONLY if absolutely necessary and AFTER validation.
    //Example using PDO prepared statement: (Best practice)

    $db = new PDO('mysql:host=localhost;dbname=mydb', 'user', 'password');
    $stmt = $db->prepare("INSERT INTO users (username, email, password) VALUES (?, ?, ?)");
    $stmt->execute([$userData['username'], $userData['email'], password_hash($userData['password'], PASSWORD_DEFAULT)]);

    return true;
}


$userData = $_POST; //Example from a POST request. Never trust user input.

if (registerUser($userData)) {
    echo "User registered successfully!";
} else {
    echo "Registration failed. Please check your input.";
}

?>
```

**Bad Code:**

```php
<?php

function registerUserBad($username, $email, $password){
    //No input validation whatsoever.  Directly uses user supplied data.
    $username = mysql_real_escape_string($username); //Deprecated function; vulnerable to SQL injection in many cases
    $email = mysql_real_escape_string($email);
    $password = mysql_real_escape_string($password); //Sanitization instead of prevention; insufficient protection
    
    $sql = "INSERT INTO users (username, email, password) VALUES ('$username', '$email', '$password')";
    mysql_query($sql); //Deprecated; vulnerable to SQL injection
}


$username = $_POST['username'];
$email = $_POST['email'];
$password = $_POST['password'];
registerUserBad($username, $email, $password);

?>
```

**Key Takeaways:**

* **Prevention over Sanitization:** The good code prioritizes validating input *before* any processing occurs, preventing potentially harmful data from ever entering the system.  The bad code relies on sanitization which is prone to failure and incomplete.
* **Input Validation:**  The good code thoroughly checks the input data's type, format, and length. The bad code lacks any validation.
* **Prepared Statements:** The good code uses prepared statements (with PDO), preventing SQL injection vulnerabilities. The bad code uses deprecated and vulnerable functions, directly embedding user input in the SQL query, a prime example of SQL injection.
* **Security:** The good code employs password hashing using `password_hash()`, making it significantly more secure against brute-force attacks.  The bad code stores the password without any hashing, which poses a severe security risk.
* **Error Handling:** The good code incorporates error handling and clearly communicates success or failure. The bad code lacks explicit error handling, making debugging and security monitoring challenging.
* **Modern Practices:**  The good code uses modern, secure functions and libraries (PDO), while the bad code relies on outdated and insecure functions (`mysql_*`).


The "Bad Code" example demonstrates several common and dangerous pitfalls in PHP input handling, highlighting the critical importance of employing a prevention-first approach.  The "Good Code" example showcases best practices resulting in significantly more secure code.
