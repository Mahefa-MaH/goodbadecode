**Title:** PHP: Secure File Upload Handling - Good vs. Bad

**Summary:**  The key difference lies in secure handling of file uploads; good code validates file types, sizes, and names to prevent vulnerabilities, while bad code leaves the server open to attacks and unexpected file types.

**Good Code:**

```php
<?php

// Allowed file types
$allowedTypes = ['image/jpeg', 'image/png', 'image/gif'];

// Max file size in bytes
$maxSize = 5 * 1024 * 1024; // 5MB

// Check if file was uploaded
if (isset($_FILES['uploadedFile']) && $_FILES['uploadedFile']['error'] === UPLOAD_ERR_OK) {
    $tempFile = $_FILES['uploadedFile']['tmp_name'];
    $fileType = $_FILES['uploadedFile']['type'];
    $fileSize = $_FILES['uploadedFile']['size'];
    $fileName = $_FILES['uploadedFile']['name'];

    // Validate file type
    if (!in_array($fileType, $allowedTypes)) {
        die("Invalid file type.");
    }

    // Validate file size
    if ($fileSize > $maxSize) {
        die("File too large.");
    }

    // Sanitize file name (prevent directory traversal)
    $fileName = basename($fileName); //Removes any path information
    $fileName = preg_replace('/[^a-zA-Z0-9._-]/', '', $fileName); //Removes unwanted characters

    // Generate a unique file name to avoid overwriting
    $uniqueFileName = uniqid() . "_" . $fileName;

    // Define upload directory, make sure it's writable
    $uploadDir = 'uploads/';
    if (!is_dir($uploadDir)) {
        mkdir($uploadDir, 0755, true); // creates directory if it doesn't exist; adjust permissions as needed
    }
    $uploadPath = $uploadDir . $uniqueFileName;


    // Move the uploaded file
    if (move_uploaded_file($tempFile, $uploadPath)) {
        echo "File uploaded successfully.";
    } else {
        die("Error uploading file.");
    }
} else {
    echo "No file uploaded.";
}


?>
```

**Bad Code:**

```php
<?php
    if(isset($_FILES["uploadedFile"])){
        move_uploaded_file($_FILES["uploadedFile"]["tmp_name"], $_FILES["uploadedFile"]["name"]);
        echo "File uploaded successfully.";
    }
?>
```


**Key Takeaways:**

* **Input Validation:** The good code rigorously checks the file type, size, and name, preventing malicious uploads. The bad code lacks any validation, making it vulnerable to various attacks.
* **File Name Sanitization:**  The good code sanitizes the filename to prevent directory traversal attacks, where a malicious user tries to access files outside the intended upload directory.  The bad code directly uses the uploaded filename, leading to potential security breaches.
* **Error Handling:** The good code provides informative error messages and handles potential errors (e.g., file size limits, invalid file types). The bad code lacks proper error handling.
* **Unique File Names:** The good code uses `uniqid()` to generate a unique filename, avoiding overwriting existing files.  The bad code risks overwriting files with the same name.
* **Directory Creation and Permissions:** The good code safely creates the upload directory if it doesn't exist and sets appropriate permissions, preventing potential permission-related errors. The bad code assumes the directory exists and is writable.
* **Security:** The good code significantly reduces the risk of security vulnerabilities like file injection, directory traversal, and denial-of-service attacks.  The bad code is highly vulnerable to all of these.

Remember to adjust file permissions (`0755` in the example) according to your server's security policy.  Always prioritize secure coding practices when handling user-uploaded files.
