-- Good Code Example:  Using CTE for readability and maintainability.
WITH RankedSales AS (
    SELECT
        ProductName,
        SalesAmount,
        RANK() OVER (ORDER BY SalesAmount DESC) AS SalesRank
    FROM
        SalesTable
)
SELECT
    ProductName,
    SalesAmount
FROM
    RankedSales
WHERE
    SalesRank <= 5;


-- Bad Code Example:  Lack of clarity, inline calculations, and poor formatting.
SELECT ProductName,SalesAmount FROM SalesTable ORDER BY SalesAmount DESC select top 5 ProductName,SalesAmount from SalesTable order by SalesAmount desc;
