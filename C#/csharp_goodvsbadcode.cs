// Good Code Example: Using Async/Await for I/O-bound operations

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
            string result = await GetWebsiteContentAsync(httpClient, "https://www.example.com");
            Console.WriteLine(result.Substring(0, 100)); //Print first 100 chars
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

    private static async Task<string> GetWebsiteContentAsync(HttpClient client, string url)
    {
        HttpResponseMessage response = await client.GetAsync(url);
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
}


// Bad Code Example: Blocking I/O and poor error handling.

using System;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        try
        {
            using (var client = new WebClient())
            {
                string result = client.DownloadString("https://www.example.com");
                Console.WriteLine(result.Substring(0,100)); //Print first 100 chars
            }
        }
        catch (Exception)
        {
            Console.WriteLine("Something went wrong!"); // Vague error message
        }
    }
}
