// Good Code: Using a more advanced pattern like the Strategy pattern for flexible logging.

public interface ILogger
{
    void Log(string message);
}

public class ConsoleLogger : ILogger
{
    public void Log(string message) => Console.WriteLine($"Console: {message}");
}

public class FileLogger : ILogger
{
    private readonly string _filePath;

    public FileLogger(string filePath) => _filePath = filePath;

    public void Log(string message)
    {
        File.AppendAllText(_filePath, $"File: {message}{Environment.NewLine}");
    }
}


public class MyClass
{
    private readonly ILogger _logger;

    public MyClass(ILogger logger) => _logger = logger;

    public void DoSomething()
    {
        _logger.Log("Something happened!"); 
    }
}


// Usage
var consoleLogger = new ConsoleLogger();
var fileLogger = new FileLogger("log.txt");

var myClassConsole = new MyClass(consoleLogger);
var myClassFile = new MyClass(fileLogger);

myClassConsole.DoSomething();
myClassFile.DoSomething();



// Bad Code:  Illustrates tight coupling and lack of error handling.

public class BadClass
{
    public void DoSomethingElse()
    {
        try
        {
            // Simulate potential file IO error
            File.WriteAllText("somefile.txt", "Some text");
            Console.WriteLine("File written successfully (Bad Code)");
        }
        catch (Exception ex)
        {
            //Poor error handling -  just swallowing the exception.
        }

        //Directly accessing database, lacks parameterization for SQL injection vulnerability.  
        string connectionString = "Data Source=.;Initial Catalog=MyDatabase;Integrated Security=True";
        using (SqlConnection connection = new SqlConnection(connectionString))
        {
            connection.Open();
            string sql = "SELECT * FROM MyTable WHERE id = '" + someUserInput + "'"; //SQL Injection vulnerability!
            using (SqlCommand command = new SqlCommand(sql, connection))
            {
                using (SqlDataReader reader = command.ExecuteReader())
                {
                    //Process the reader
                }
            }
        }
    }
}

//Illustrates lack of input validation
string someUserInput = Console.ReadLine();
var badClassInstance = new BadClass();
badClassInstance.DoSomethingElse();


