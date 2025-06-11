// Good Code: Using async/await for asynchronous operations and leveraging C# features.
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public async Task<string> GetWebsiteContentAsync(string url)
    {
        using (var httpClient = new HttpClient())
        {
            try
            {
                var response = await httpClient.GetAsync(url);
                response.EnsureSuccessStatusCode(); // Throw if not a success code.
                return await response.Content.ReadAsStringAsync();
            }
            catch (HttpRequestException ex)
            {
                //Robust error handling. Log or handle exception appropriately.
                Console.WriteLine($"Error fetching URL: {ex.Message}");
                return null; // Or throw a custom exception for better control
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An unexpected error occurred: {ex.Message}");
                return null;
            }
        }
    }
}


// Bad Code:  Ignoring exceptions, blocking calls, poor error handling, and lack of async.
public class BadCodeExample
{
    public string GetWebsiteContent(string url)
    {
        try
        {
            var httpClient = new HttpClient();
            var response = httpClient.GetAsync(url).Result; // Blocking call, prone to deadlocks.
            return response.Content.ReadAsStringAsync().Result; //Another blocking call.
        }
        catch
        {
            return ""; // Swallowing exceptions - Very bad practice!
        }
    }
}
