<?php

// Good Code Example: Using a well-structured class for database interaction

class Database {
    private $pdo;

    public function __construct($host, $dbname, $user, $password) {
        try {
            $this->pdo = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
            $this->pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        } catch (PDOException $e) {
            die("Database connection failed: " . $e->getMessage());
        }
    }

    public function query($sql, $params = []) {
        $stmt = $this->pdo->prepare($sql);
        $stmt->execute($params);
        return $stmt->fetchAll(PDO::FETCH_ASSOC);
    }
}


// Bad Code Example: Directly embedding database credentials and SQL in the code

$host = "localhost";
$dbname = "mydatabase";
$user = "myuser";
$password = "mypassword";

$conn = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
$sql = "SELECT * FROM users WHERE username = '" . $_GET['username'] . "'"; //SQL Injection vulnerability
$result = $conn->query($sql);
foreach ($result as $row) {
    echo $row['username'];
}


?>
