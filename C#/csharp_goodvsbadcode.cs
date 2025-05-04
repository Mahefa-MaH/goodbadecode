// Good Code Example: Using Async/Await and proper exception handling for a web request

using System;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public static async Task Main(string[] args)
    {
        using (var httpClient = new HttpClient())
        {
            try
            {
                var response = await httpClient.GetAsync("https://www.example.com");
                response.EnsureSuccessStatusCode(); // Throw an exception for bad status codes
                var content = await response.Content.ReadAsStringAsync();
                Console.WriteLine(content);
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
}


// Bad Code Example: Ignoring exceptions and using synchronous web requests

using System;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        try
        {
            var request = WebRequest.Create("https://www.example.com");
            using (var response = request.GetResponse())
            using (var reader = new System.IO.StreamReader(response.GetResponseStream()))
            {
                string content = reader.ReadToEnd();
                Console.WriteLine(content);
            }
        }
        catch (Exception) {  //Ignoring exceptions is bad practice
            //Do nothing - this is bad practice
        }

    }
}
