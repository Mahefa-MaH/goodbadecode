// Good Code: Uses asynchronous programming for I/O-bound operations, leverages built-in exception handling, and implements proper resource management.

using System;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public static async Task Main(string[] args)
    {
        try
        {
            using (var httpClient = new HttpClient())
            {
                var response = await httpClient.GetAsync("https://www.example.com");
                response.EnsureSuccessStatusCode();
                var content = await response.Content.ReadAsStringAsync();
                Console.WriteLine(content);
            }
        }
        catch (HttpRequestException ex)
        {
            Console.WriteLine($"HTTP Request Error: {ex.Message}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"An unexpected error occurred: {ex.Message}");
        }
    }
}


// Bad Code: Lacks error handling, uses synchronous I/O blocking the main thread, and doesn't manage resources properly.

using System;
using System.IO;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        try
        {
            var request = WebRequest.Create("https://www.example.com");
            using (var response = request.GetResponse())
            using (var stream = response.GetResponseStream())
            using (var reader = new StreamReader(stream))
            {
                Console.WriteLine(reader.ReadToEnd());
            }
        }
        catch (Exception ex)
        {
            //This is a very poor error handling
            Console.WriteLine("Error: " + ex.Message);
        }
    }
}
