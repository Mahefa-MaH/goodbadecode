<?php

// Good Code: Using a dedicated class for database interaction

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


//Example usage
$db = new Database('localhost', 'mydatabase', 'user', 'password');
$users = $db->query("SELECT * FROM users WHERE active = 1");

foreach ($users as $user) {
    echo $user['username'] . "<br>";
}



// Bad Code: Directly embedding database credentials and SQL queries in the code.

$host = 'localhost';
$dbname = 'mydatabase';
$user = 'user';
$password = 'password';

try {
    $pdo = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $stmt = $pdo->query("SELECT * FROM users WHERE active = 1");
    $users = $stmt->fetchAll(PDO::FETCH_ASSOC);

    foreach ($users as $user) {
        echo $user['username'] . "<br>";
    }
} catch (PDOException $e) {
    die("Database connection failed: " . $e->getMessage());
}

?>
