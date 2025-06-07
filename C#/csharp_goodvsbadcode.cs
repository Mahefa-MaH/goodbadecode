// Good Code: Using asynchronous programming for I/O-bound operations.
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
                return await httpClient.GetStringAsync(url);
            }
            catch (HttpRequestException ex)
            {
                //Robust error handling.  Log the exception and return a user-friendly message.
                Console.Error.WriteLine($"Error accessing {url}: {ex.Message}");
                return $"Error: Could not retrieve content from {url}";
            }
        }
    }
}


// Bad Code: Blocking the main thread for I/O-bound operations.
using System;
using System.Net;

public class BadCodeExample
{
    public string GetWebsiteContent(string url)
    {
        using (var webClient = new WebClient())
        {
            try
            {
                return webClient.DownloadString(url);
            }
            catch (WebException ex)
            {
                //Poor error handling.  Simply returns the exception message to the user.
                return ex.Message;
            }
        }
    }
}
