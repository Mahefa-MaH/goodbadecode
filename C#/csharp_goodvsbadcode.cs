// Good Code: Using asynchronous programming for I/O-bound operations.
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class GoodCodeExample
{
    public async Task<string> DownloadStringAsync(string url)
    {
        using (var httpClient = new HttpClient())
        {
            return await httpClient.GetStringAsync(url);
        }
    }
}


// Bad Code: Blocking the main thread during I/O-bound operations.
using System;
using System.Net;

public class BadCodeExample
{
    public string DownloadString(string url)
    {
        using (var webClient = new WebClient())
        {
            return webClient.DownloadString(url);
        }
    }
}

//Good Code: Implementing IDisposable for proper resource management.

public class GoodResourceManagement : IDisposable
{
    private readonly System.IO.Stream _stream;

    public GoodResourceManagement(string filePath)
    {
        _stream = new System.IO.FileStream(filePath, System.IO.FileMode.Open);
    }


    public void Dispose()
    {
        _stream?.Dispose();
    }
}


//Bad Code: Failing to implement IDisposable, leading to resource leaks.

public class BadResourceManagement
{
    private readonly System.IO.Stream _stream;

    public BadResourceManagement(string filePath)
    {
        _stream = new System.IO.FileStream(filePath, System.IO.FileMode.Open);
    }
}

