**Title:** Secure & Efficient PL/SQL: Handling External Data

**Summary:**  Efficient PL/SQL code uses parameterized queries to prevent SQL injection vulnerabilities and robust exception handling for reliable data processing, unlike insecure, inefficient code that's prone to errors and security breaches.

**Good Code:**

```sql
CREATE OR REPLACE PROCEDURE get_data (p_id IN NUMBER, p_data OUT SYS_REFCURSOR) AS
BEGIN
  OPEN p_data FOR
    SELECT *
    FROM my_table
    WHERE id = p_id;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    DBMS_OUTPUT.PUT_LINE('No data found for ID: ' || p_id);
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('An error occurred: ' || SQLERRM);
END;
/
```

**Bad Code:**

```sql
CREATE OR REPLACE PROCEDURE get_data_bad (p_id IN VARCHAR2) AS
BEGIN
  EXECUTE IMMEDIATE 'SELECT * FROM my_table WHERE id = ' || p_id;
END;
/
```


**Key Takeaways:**

* **Security:** The "Good Code" uses parameterized queries (`p_id IN NUMBER`), preventing SQL injection vulnerabilities. The "Bad Code" directly concatenates user input into the SQL statement, making it highly susceptible to attacks.
* **Error Handling:** The "Good Code" includes comprehensive exception handling (`EXCEPTION` block), gracefully handling potential errors (e.g., `NO_DATA_FOUND`, `OTHERS`). The "Bad Code" lacks error handling, leading to unpredictable behavior and potential crashes.
* **Efficiency:** While not drastically different in this simple example, parameterized queries generally lead to better query optimization by the database, improving performance, especially with complex queries.
* **Data Type Safety:** The "Good Code" explicitly defines the data type of the input parameter (`p_id IN NUMBER`), improving data integrity and preventing type-related errors.  The "Bad Code" uses a generic `VARCHAR2`, which can lead to implicit conversions and unexpected results.
* **Maintainability and Readability:**  The "Good Code" is more readable and easier to maintain due to its structured approach and clear error handling.


