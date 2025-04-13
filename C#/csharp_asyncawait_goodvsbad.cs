//Good Code: Using Async/Await for I/O-bound operations
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public static async Task Main(string[] args)
    {
        var client = new HttpClient();
        try
        {
            string result = await GetWebsiteContentAsync(client, "https://www.example.com");
            Console.WriteLine(result);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
        finally
        {
            client.Dispose();
        }
    }

    public static async Task<string> GetWebsiteContentAsync(HttpClient client, string url)
    {
        HttpResponseMessage response = await client.GetAsync(url);
        response.EnsureSuccessStatusCode();
        return await response.Content.ReadAsStringAsync();
    }
}


//Bad Code: Blocking I/O-bound operations on main thread
using System;
using System.Net;

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        using (var client = new WebClient())
        {
            try
            {
                string result = client.DownloadString("https://www.example.com");
                Console.WriteLine(result);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
    }
}
