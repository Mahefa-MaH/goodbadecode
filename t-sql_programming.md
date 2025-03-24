**Title:**  Efficient vs. Inefficient T-SQL: Handling NULL Values

**Summary:**  Efficient T-SQL handles NULLs gracefully using ISNULL or COALESCE for predictable results, while inefficient code often leads to unexpected behavior or errors when NULLs are encountered in comparisons or calculations.

**Good Code:**

```sql
-- Efficiently handles NULLs in a calculation
SELECT 
    ISNULL(order_total, 0) + ISNULL(shipping_cost, 0) AS total_cost
FROM Orders;

-- Efficiently handles NULLs in a WHERE clause
SELECT *
FROM Customers
WHERE ISNULL(email, '') <> '';


--Using COALESCE for multiple choices
SELECT COALESCE(column1, column2, column3, 'Default Value') AS result FROM MyTable;
```

**Bad Code:**

```sql
-- Inefficient:  Direct comparison with NULL leads to unpredictable results.
SELECT *
FROM Orders
WHERE order_total + shipping_cost > 100; --Fails if either order_total or shipping_cost is NULL


-- Inefficient:  Potentially causes unexpected NULL results.
SELECT
    order_total * shipping_cost AS total_cost
FROM Orders; -- Results in NULL if either order_total or shipping_cost is NULL

```

**Key Takeaways:**

* **Avoids unexpected NULL behavior:** The `ISNULL` and `COALESCE` functions provide explicit handling of NULL values, preventing unexpected results or errors in calculations and comparisons.  Direct comparison with NULL usually yields unexpected results in T-SQL.
* **Improved readability and maintainability:** Explicitly handling NULLs makes the code easier to understand and maintain.  The intent is clear, reducing ambiguity.
* **Data Integrity:** Using `ISNULL` or `COALESCE` ensures that calculations and comparisons are performed using a predictable default value, improving data integrity.
* **Performance:**  While the difference might be marginal in small queries, handling NULLs effectively can lead to performance improvements, especially in complex queries involving joins and aggregations, by reducing the need for the database engine to handle unexpected NULL results.


