**Title:** Efficient vs. Inefficient SQL Queries: A Comparative Analysis

**Summary:**  The key difference lies in query optimization: efficient SQL leverages indexing and avoids full table scans, while inefficient SQL leads to slow performance and resource exhaustion.


**Good Code:**

```sql
-- Efficient query using an index on 'customer_id'
SELECT order_id, order_date, total_amount
FROM orders
WHERE customer_id = 12345;

-- Efficient query using JOIN instead of subqueries for better performance
SELECT o.order_id, c.customer_name, o.order_date
FROM orders o
JOIN customers c ON o.customer_id = c.customer_id
WHERE o.order_date >= '2024-01-01';

--Efficient use of LIMIT to avoid unnecessary data retrieval
SELECT * FROM large_table LIMIT 100;
```

**Bad Code:**

```sql
-- Inefficient query: full table scan on a large table without index
SELECT * 
FROM orders
WHERE customer_id = 12345;

-- Inefficient query: using multiple subqueries instead of JOIN
SELECT order_id, order_date, total_amount
FROM orders
WHERE customer_id IN (SELECT customer_id FROM customers WHERE city = 'New York');


--Inefficient query: Selecting all columns without specifying needed ones
SELECT * FROM large_table WHERE condition = true;

-- Inefficient use of wildcard in the beginning of a LIKE clause
SELECT * FROM products WHERE product_name LIKE '%widget';
```


**Key Takeaways:**

* **Index Usage:**  The good code utilizes indexes (implied or explicitly created) to drastically speed up searches. The bad code performs full table scans, which are extremely slow on large datasets.

* **JOINs vs. Subqueries:**  `JOIN` operations are generally more efficient than nested subqueries, especially with multiple joins. The good code exemplifies this best practice.

* **SELECT Specific Columns:** Only retrieve the columns you actually need. Selecting `*` can be extremely inefficient, especially with large tables.

* **Avoid Wildcard at the Beginning of LIKE:**  Using `LIKE '%widget'` forces a full table scan. More specific search criteria will significantly speed up queries.

* **LIMIT Clause:** Using `LIMIT` restricts the number of rows returned, improving query performance, especially for large datasets where you only need a subset of the data.


* **Prepared Statements:** For dynamic SQL queries (where parts of the query change), use parameterized queries (prepared statements) to prevent SQL injection vulnerabilities.  This is not explicitly shown, but crucial for secure code.  The good code *implicitly* assumes prepared statements would be used for dynamic parameters.  Always sanitize user input and never directly concatenate user input into SQL queries.
