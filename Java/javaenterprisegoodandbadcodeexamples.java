// Good Code Example: Using a DAO for database interaction
public interface UserDAO {
    User getUserById(int id);
    void createUser(User user);
    void updateUser(User user);
    void deleteUser(int id);
}

public class UserDAOImpl implements UserDAO {
    //Implementation using JDBC or ORM like Hibernate
    @Override
    public User getUserById(int id) {
        //Implementation to fetch user from database
        return null;
    }

    @Override
    public void createUser(User user) {
        //Implementation to create user in database
    }

    @Override
    public void updateUser(User user) {
        //Implementation to update user in database
    }

    @Override
    public void deleteUser(int id) {
        //Implementation to delete user from database
    }
}

public class UserService {
    private UserDAO userDAO;

    public UserService(UserDAO userDAO) {
        this.userDAO = userDAO;
    }

    public User getUser(int id) {
        return userDAO.getUserById(id);
    }

    public void registerUser(User user){
        userDAO.createUser(user);
    }
}


// Bad Code Example: Database interaction directly in a service class
public class BadUserService {
    //Direct database connection and query in service class.  This is bad practice.
    public User getUser(int id) {
        // Database connection and query here... highly discouraged
        return null;
    }
}

