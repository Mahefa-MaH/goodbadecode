// Good Code: Using generics for type safety and efficiency
public class GoodCodeExample<T> where T : IComparable<T>
{
    public T FindMax(T[] array)
    {
        if (array == null || array.Length == 0)
        {
            throw new ArgumentException("Array cannot be null or empty.");
        }
        T max = array[0];
        for (int i = 1; i < array.Length; i++)
        {
            if (array[i].CompareTo(max) > 0)
            {
                max = array[i];
            }
        }
        return max;
    }
}


// Bad Code: Lack of error handling and inefficient string concatenation
public class BadCodeExample
{
    public string ConcatenateStrings(string[] strings)
    {
        string result = "";
        foreach (string str in strings)
        {
            result += str; // Inefficient string concatenation
        }
        return result;
    }
}

//Good Code: Using async/await for asynchronous operations.
public class GoodAsyncExample
{
    public async Task<string> DownloadDataAsync(string url)
    {
        using (var client = new HttpClient())
        {
            return await client.GetStringAsync(url);
        }
    }
}

//Bad Code: Blocking the UI thread during an asynchronous operation.
public class BadAsyncExample
{
    public void DownloadData(string url)
    {
        using (var client = new HttpClient())
        {
            string data = client.GetStringAsync(url).Result; //Blocks the UI thread
            //Process data
        }
    }
}


//Good Code:  Utilizing Dependency Injection for better testability and maintainability.
public interface IEmailService
{
    void SendEmail(string to, string subject, string body);
}

public class EmailService : IEmailService
{
    public void SendEmail(string to, string subject, string body)
    {
        //Implementation for sending email
        Console.WriteLine($"Email sent to: {to}, Subject: {subject}, Body: {body}");
    }
}

public class UserService
{
    private readonly IEmailService _emailService;

    public UserService(IEmailService emailService)
    {
        _emailService = emailService;
    }

    public void RegisterUser(string email)
    {
        _emailService.SendEmail(email, "Welcome", "Thank you for registering!");
    }
}

//Bad Code: Tight coupling and difficult to test
public class BadUserService
{
    public void RegisterUser(string email)
    {
        //Implementation directly calls email sending logic.
        //Difficult to test and maintain.
        SendEmail(email,"Welcome","Thank you for registering!");

    }

    private void SendEmail(string to, string subject, string body)
    {
        //Email sending implementation
        Console.WriteLine($"Email sent to: {to}, Subject: {subject}, Body: {body}");
    }
}
