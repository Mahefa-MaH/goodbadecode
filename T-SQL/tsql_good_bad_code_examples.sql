-- Good Code Example:  Uses parameterized queries to prevent SQL injection and is well-formatted.
CREATE PROCEDURE GetUsersByName (@name VARCHAR(50))
AS
BEGIN
    SELECT * FROM Users WHERE Name LIKE '%' + @name + '%';
END;
GO

-- Bad Code Example: Vulnerable to SQL injection and poorly formatted.
CREATE PROCEDURE GetUsersByNameBad ( @name VARCHAR(50) )
as
begin
select * from Users where Name like '%' + @name + '%'
end
GO

--Good Code Example: Uses appropriate data types and error handling.
CREATE PROCEDURE UpdateUser (@UserID INT, @NewEmail VARCHAR(100))
AS
BEGIN
    BEGIN TRANSACTION;
    BEGIN TRY
        UPDATE Users SET Email = @NewEmail WHERE UserID = @UserID;
        COMMIT TRANSACTION;
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;
        THROW;
    END CATCH;
END;
GO

--Bad Code Example:  Doesn't handle errors, uses implicit data type conversion, and lacks transactions.
CREATE PROCEDURE UpdateUserBad (@UserID INT, @NewEmail VARCHAR(100))
AS
BEGIN
    UPDATE Users SET Email = @NewEmail WHERE UserID = @UserID
END
GO


--Good Code Example: Efficiently retrieves data using joins
SELECT u.FirstName, u.LastName, o.OrderDate
FROM Users u
INNER JOIN Orders o ON u.UserID = o.UserID
WHERE o.OrderDate >= '20230101';
GO

--Bad Code Example: Inefficient data retrieval using subqueries.
SELECT FirstName,LastName, (SELECT OrderDate FROM Orders WHERE UserID = u.UserID) as OrderDate
FROM Users u;
GO
