// Good Code: Using asynchronous programming for I/O-bound operations
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public static async Task Main(string[] args)
    {
        var httpClient = new HttpClient();
        try
        {
            var tasks = new[]
            {
                httpClient.GetStringAsync("https://www.example.com"),
                httpClient.GetStringAsync("https://www.google.com")
            };
            var results = await Task.WhenAll(tasks);
            foreach (var result in results)
            {
                Console.WriteLine(result.Length); //Process results
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
        finally
        {
            httpClient.Dispose();
        }

    }
}


// Bad Code: Blocking I/O operations and potential deadlocks.
using System;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        var webClient = new WebClient();
        try
        {
            string result1 = webClient.DownloadString("https://www.example.com");
            string result2 = webClient.DownloadString("https://www.google.com");
            Console.WriteLine(result1.Length);
            Console.WriteLine(result2.Length);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
