// Good Code: Using LINQ for efficient data processing

using System;
using System.Collections.Generic;
using System.Linq;

public class GoodCodeExample
{
    public static void Main(string[] args)
    {
        List<int> numbers = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        // Efficiently filter and process data using LINQ
        var evenNumbersSquared = numbers
            .Where(n => n % 2 == 0)
            .Select(n => n * n)
            .ToList();

        Console.WriteLine(string.Join(", ", evenNumbersSquared)); // Output: 4, 16, 36, 64, 100

        //Further processing with LINQ
        var sumOfSquares = evenNumbersSquared.Sum();
        Console.WriteLine($"Sum of squares: {sumOfSquares}"); //Output: Sum of squares: 220

    }
}


// Bad Code: Inefficient and less readable data processing

public class BadCodeExample
{
    public static void Main(string[] args)
    {
        List<int> numbers = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        List<int> evenNumbersSquared = new List<int>();

        // Inefficient loop-based approach
        foreach (int number in numbers)
        {
            if (number % 2 == 0)
            {
                evenNumbersSquared.Add(number * number);
            }
        }

        Console.WriteLine(string.Join(", ", evenNumbersSquared)); // Output: 4, 16, 36, 64, 100

        int sum =0;
        foreach(int num in evenNumbersSquared){
            sum += num;
        }
        Console.WriteLine($"Sum of squares: {sum}"); //Output: Sum of squares: 220
    }
}
