// Good Code: Using asynchronous programming for I/O-bound operations.
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
                Console.WriteLine(result.Substring(0, 100)); //Process only a portion to avoid excessive output
            }
        }
        catch (HttpRequestException ex)
        {
            Console.WriteLine($"Request error: {ex.Message}");
        }
        finally
        {
            httpClient.Dispose();
        }
    }
}


// Bad Code: Blocking I/O operations, prone to deadlocks and unresponsive UI.
using System;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        using (var webClient = new WebClient())
        {
            try
            {
                string result1 = webClient.DownloadString("https://www.example.com");
                string result2 = webClient.DownloadString("https://www.google.com");
                Console.WriteLine(result1.Substring(0,100));
                Console.WriteLine(result2.Substring(0,100));

            }
            catch (WebException ex)
            {
                Console.WriteLine($"Request error: {ex.Message}");
            }
        }
    }
}
