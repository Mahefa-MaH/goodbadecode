-- Good SQL Code Example:  Retrieving data with clear column selection and WHERE clause.
SELECT order_id, customer_id, order_date
FROM orders
WHERE order_date >= '2023-01-01' AND order_date <= '2023-12-31';

-- Bad SQL Code Example:  Unclear column selection, implicit data type conversion, and inefficient JOIN.
SELECT *
FROM orders o, customers c
WHERE o.customer_id = c.id AND o.order_date LIKE '%2023%';

-- Good SQL Code Example:  Using parameterized queries to prevent SQL injection.
-- (Requires specific database driver/library support)
PREPARED STATEMENT:
SELECT order_id, customer_id, order_date FROM orders WHERE order_date >= ? AND order_date <= ?;

-- Bad SQL Code Example:  SQL injection vulnerability.
SELECT order_id, customer_id, order_date FROM orders WHERE order_date >= '2023-01-01' AND order_date <= '2023-12-31' AND customer_id = '" + userInput + "'";

-- Good SQL Code Example:  Using indexes for efficient query performance.  (Assumes index exists on order_date)
SELECT order_id, customer_id, order_date
FROM orders
WHERE order_date >= '2023-01-01';

-- Bad SQL Code Example:  Lack of indexes, leading to slow query performance on large tables. (No index on order_date)
SELECT order_id, customer_id, order_date
FROM orders
WHERE order_date >= '2023-01-01';


-- Good SQL Code Example:  Proper use of transactions for data integrity.
BEGIN TRANSACTION;
UPDATE accounts SET balance = balance - 100 WHERE account_id = 1;
UPDATE accounts SET balance = balance + 100 WHERE account_id = 2;
COMMIT;

-- Bad SQL Code Example:  Lack of transactions, potentially leading to inconsistent data.
UPDATE accounts SET balance = balance - 100 WHERE account_id = 1;
UPDATE accounts SET balance = balance + 100 WHERE account_id = 2; --Could fail halfway through.

