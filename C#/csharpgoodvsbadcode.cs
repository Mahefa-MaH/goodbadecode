using System;
using System.Collections.Generic;
using System.Linq;

public class GoodCodeExample
{
    public static double CalculateAverage(IEnumerable<double> numbers)
    {
        if (numbers == null || !numbers.Any())
        {
            throw new ArgumentException("Input sequence cannot be null or empty.");
        }

        return numbers.Average();
    }

    public static void Main(string[] args)
    {
        var numbers = new List<double> { 1.0, 2.5, 3.7, 4.2, 5.9 };
        double average = CalculateAverage(numbers);
        Console.WriteLine($"Average: {average}");


        try
        {
            double avg = CalculateAverage(null);
            Console.WriteLine($"Average: {avg}");
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}


public class BadCodeExample
{
    public static double CalculateAverage(List<double> numbers) // No null check and less flexible
    {
        double sum = 0;
        foreach (double number in numbers)
        {
            sum += number;
        }
        return sum / numbers.Count; // Potential DivideByZeroException
    }
    public static void Main(string[] args)
    {
        var numbers = new List<double> { 1.0, 2.5, 3.7, 4.2, 5.9 };
        double average = CalculateAverage(numbers);
        Console.WriteLine($"Average: {average}");


        try
        {
            double avg = CalculateAverage(new List<double>()); //Potential Exception
            Console.WriteLine($"Average: {avg}");
        }
        catch (DivideByZeroException ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
}
