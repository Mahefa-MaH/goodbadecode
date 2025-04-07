**Title:** Efficient String Manipulation in PHP: Secure vs. Insecure Approaches

**Summary:**  The key difference lies in using prepared statements to prevent SQL injection vulnerabilities when handling user-supplied data in database queries,  and employing more efficient string functions to avoid unnecessary overhead.

**Good Code:**

```php
<?php

// Secure database interaction using prepared statements
function getUserName($userId, $pdo){
    $stmt = $pdo->prepare("SELECT username FROM users WHERE id = ?");
    $stmt->execute([$userId]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);
    return isset($user['username']) ? $user['username'] : null;
}

// Efficient string manipulation
function formatName($firstName, $lastName){
    $firstName = trim(mb_ucfirst(strtolower($firstName))); // handles multibyte characters
    $lastName = trim(mb_ucfirst(strtolower($lastName)));
    return $firstName . " " . $lastName;
}

// Example usage (assuming PDO connection is established as $pdo)
$userId = $_GET['id']; // Get user ID from GET request
$userName = getUserName($userId, $pdo);
if ($userName) {
  echo "User Name: " . $userName . "<br>";
} else {
  echo "User not found.<br>";
}


$firstName = $_POST['firstName'];
$lastName = $_POST['lastName'];
echo "Formatted Name: " . formatName($firstName, $lastName);


?>
```

**Bad Code:**

```php
<?php

// Vulnerable to SQL injection
function getUserNameInsecure($userId){
    $query = "SELECT username FROM users WHERE id = '$userId'";
    $result = mysql_query($query); // Deprecated and insecure!
    if ($row = mysql_fetch_assoc($result)){
        return $row['username'];
    } else {
        return null;
    }
}

// Inefficient string manipulation
function formatNameInefficient($firstName, $lastName){
    $firstName = ucfirst(strtolower($firstName));
    $lastName = ucfirst(strtolower($lastName));
    return $firstName . " " . $lastName; //No trimming, potential whitespace issues.
}

// Example usage (highly vulnerable!)
$userId = $_GET['id'];
$userName = getUserNameInsecure($userId);
echo "User Name: " . $userName;

$firstName = $_POST['firstName'];
$lastName = $_POST['lastName'];
echo "Formatted Name: " . formatNameInefficient($firstName, $lastName);

?>
```


**Key Takeaways:**

* **SQL Injection Prevention:** The good code uses prepared statements, a crucial security measure to prevent SQL injection attacks by separating data from SQL code.  The bad code directly incorporates user input into the SQL query, making it extremely vulnerable.  Using a modern database library like PDO is also recommended over deprecated functions like `mysql_*`.
* **Efficient String Handling:** The good code utilizes `mb_ucfirst` and `mb_strtolower` for better handling of multibyte characters (important for internationalization) and `trim` to remove leading/trailing whitespace. The bad code lacks these features resulting in less robust and potentially inaccurate output.
* **Error Handling:** The good code includes more robust error handling (checking if the user exists), whereas the bad code lacks comprehensive error checks, which can lead to unexpected behavior or crashes.
* **Code Maintainability:**  The good code is cleaner, more readable, and easier to maintain due to its modular design and use of meaningful function names.
* **Security Best Practices:** The good code follows secure coding practices to prevent vulnerabilities, while the bad code exposes serious security risks.  Never directly embed user-supplied data into SQL queries.


