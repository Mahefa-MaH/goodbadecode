using System;
using System.Collections.Generic;
using System.Linq;

public class GoodCodeExample
{
    public static double CalculateAverage(IEnumerable<double> numbers)
    {
        if (numbers == null || !numbers.Any())
        {
            throw new ArgumentException("Input cannot be null or empty.");
        }
        return numbers.Average();
    }

    public static void Main(string[] args)
    {
        var numbers = new List<double> { 1.0, 2.5, 3.7, 4.2, 5.9 };
        try
        {
            var average = CalculateAverage(numbers);
            Console.WriteLine($"Average: {average}");
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }

        //Demonstrates using LINQ for more complex scenarios
        var evenNumbers = numbers.Where(num => num % 2 == 0);
        Console.WriteLine($"Even numbers: {string.Join(", ", evenNumbers)}");

    }
}


public class BadCodeExample
{
    public static double CalculateAverage(List<double> numbers)
    {
        double sum = 0;
        foreach (var number in numbers)
        {
            sum += number;
        }
        return sum / numbers.Count; //Potential DivideByZeroException if numbers is empty.
    }

    public static void Main(string[] args)
    {
        List<double> numbers = null; //Uninitialized List.
        double average = CalculateAverage(numbers); //This will throw a NullReferenceException.
        Console.WriteLine("Average: " + average);
    }
}
