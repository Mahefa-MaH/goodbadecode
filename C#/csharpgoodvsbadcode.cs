// Good Code: Using a dedicated class for data and leveraging LINQ for efficient querying.

public class Product
{
    public int Id { get; set; }
    public string Name { get; set; }
    public decimal Price { get; set; }
}

public class GoodCodeExample
{
    public static void Main(string[] args)
    {
        List<Product> products = new List<Product>()
        {
            new Product { Id = 1, Name = "Product A", Price = 10.99m },
            new Product { Id = 2, Name = "Product B", Price = 25.50m },
            new Product { Id = 3, Name = "Product C", Price = 5.75m }
        };

        var expensiveProducts = products.Where(p => p.Price > 10).Select(p => p.Name);

        foreach (var productName in expensiveProducts)
        {
            Console.WriteLine(productName);
        }

        //More advanced features can be added here, like using async/await for I/O bound operations, or error handling.
    }
}


// Bad Code: Mixing data and logic, using inefficient loops, and lacking error handling.

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        string[] productNames = { "Product A", "Product B", "Product C" };
        decimal[] productPrices = { 10.99m, 25.50m, 5.75m };

        List<string> expensiveProducts = new List<string>();
        for (int i = 0; i < productNames.Length; i++)
        {
            if (productPrices[i] > 10)
            {
                expensiveProducts.Add(productNames[i]);
            }
        }

        foreach (string productName in expensiveProducts)
        {
            Console.WriteLine(productName);
        }
        //Lack of error handling, inefficient code,  tight coupling.
    }
}
