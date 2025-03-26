// Good Code Example: Using JPA for database interaction

import javax.persistence.*;
import java.util.List;

@Entity
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String email;

    // Getters and setters
}

public class UserRepository extends JpaRepository<User, Long> {
}

// Bad Code Example: Directly using JDBC for database interaction (prone to SQL injection and error handling issues)

import java.sql.*;

public class UserRepositoryJdbc {
    public void createUser(String name, String email, Connection connection) throws SQLException {
        String sql = "INSERT INTO users (name, email) VALUES (?, ?)"; //SQL injection vulnerability
        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, name);
            statement.setString(2, email);
            statement.executeUpdate();
        }
    }

}
