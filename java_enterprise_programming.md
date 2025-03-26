**Title:** Efficient Resource Management in Java EE: Connection Pooling vs. Direct Connections

**Summary:**  Direct database connections lead to resource exhaustion and performance bottlenecks in Java EE applications, while connection pooling efficiently manages database connections, improving scalability and responsiveness.

**Good Code:**

```java
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class DatabaseAccess {

    public void processData() {
        try {
            // JNDI lookup for DataSource
            InitialContext ctx = new InitialContext();
            DataSource ds = (DataSource) ctx.lookup("java:comp/env/jdbc/myDataSource"); 

            try (Connection connection = ds.getConnection();
                 PreparedStatement statement = connection.prepareStatement("SELECT * FROM myTable");
                 ResultSet resultSet = statement.executeQuery()) {

                while (resultSet.next()) {
                    // Process data from resultSet
                    System.out.println(resultSet.getString("column1"));
                }
            } 
        } catch (NamingException | SQLException e) {
            // Proper exception handling - Log and potentially retry 
            e.printStackTrace(); // Replace with proper logging in production
        }
    }
}

```

**Bad Code:**

```java
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class DatabaseAccessBad {

    public void processData() {
        try {
            // Direct connection - prone to resource exhaustion
            Connection connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/mydb", "user", "password");
            PreparedStatement statement = connection.prepareStatement("SELECT * FROM myTable");
            ResultSet resultSet = statement.executeQuery();

            while (resultSet.next()) {
                // Process data
                System.out.println(resultSet.getString("column1"));
            }
            // Missing connection close - resource leak!
        } catch (SQLException e) {
            e.printStackTrace(); // Insufficient error handling
        }
    }
}
```

**Key Takeaways:**

* **Resource Management:** The good code uses a connection pool (DataSource) managed by the application server. This prevents the application from creating and holding onto a large number of database connections, avoiding resource exhaustion and improving performance under load.  The bad code creates a direct connection for each request, a major performance and scalability bottleneck.

* **Connection Closing:** The good code uses try-with-resources, ensuring that the `Connection`, `PreparedStatement`, and `ResultSet` are automatically closed, even if exceptions occur.  The bad code omits closing the connection, resulting in resource leaks that can severely impact the application and database server over time.

* **Error Handling:** The good code includes a `try-catch` block to handle potential `NamingException` (JNDI lookup failure) and `SQLException` exceptions, providing better error management.  The bad code's exception handling is minimal and doesn't address potential issues effectively.

* **Security:** The good code uses JNDI lookup to securely configure the database connection details (username, password).  The connection details are usually managed externally (e.g., in a configuration file) rather than hardcoded in the source code, improving security.  The bad code hardcodes credentials directly in the code, representing a significant security vulnerability.

* **Maintainability:** The good code is more maintainable because database connection details are managed separately from the application logic. Changes to database credentials do not necessitate code recompilation.  The bad code requires changing the code itself, increasing the risk of errors and making updates more complex.
