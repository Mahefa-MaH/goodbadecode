<?php

// Good Code Example:  Using prepared statements to prevent SQL injection
$conn = new PDO("mysql:host=localhost;dbname=mydatabase", "username", "password");
$stmt = $conn->prepare("SELECT * FROM users WHERE username = ?");
$stmt->execute([$_POST['username']]);
$user = $stmt->fetch(PDO::FETCH_ASSOC);

// Bad Code Example:  Vulnerable to SQL injection
$username = $_POST['username'];
$sql = "SELECT * FROM users WHERE username = '$username'";
$result = $conn->query($sql);
$user = $result->fetch(PDO::FETCH_ASSOC);


//Good Code Example:  Proper error handling
try {
    // Code that might throw an exception
} catch (Exception $e) {
    // Handle the exception appropriately
    error_log($e->getMessage());
    http_response_code(500);
    echo "Internal Server Error";
}


//Bad Code Example: Lack of error handling
// Code that might throw an exception without any error handling

//Good Code Example: Using a consistent coding style.
$name = "John Doe";
$age  = 30;

//Bad Code Example:Inconsistent coding style.
$NamE = "Jane Doe";
  $aGe =25;

//Good Code Example: Using functions to improve code organization and reusability.
function add($a, $b){
    return $a + $b;
}

//Bad Code Example: Code duplication and lack of functions.


?>
