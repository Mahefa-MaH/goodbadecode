using System;
using System.Collections.Generic;
using System.Linq;

public class GoodCodeExample
{
    public static double CalculateAverage(IEnumerable<double> numbers)
    {
        //Handles empty input gracefully.
        if (!numbers.Any())
        {
            return 0; // Or throw an exception, depending on requirements.
        }
        return numbers.Average();
    }

    public static void Main(string[] args)
    {
        List<double> numbers = new List<double> { 1.0, 2.0, 3.0, 4.0, 5.0 };
        double average = CalculateAverage(numbers);
        Console.WriteLine($"Average: {average}");


        //Demonstrates robustness with potential exceptions.
        List<double> numbers2 = new List<double>();
        double average2 = CalculateAverage(numbers2);
        Console.WriteLine($"Average of empty list: {average2}");

        List<double?> nullableNumbers = new List<double?> { 1.0, null, 3.0 };
        double? nullableAverage = CalculateAverageNullable(nullableNumbers);
        Console.WriteLine($"Average of nullable list: {nullableAverage}");

    }
        public static double? CalculateAverageNullable(IEnumerable<double?> numbers)
    {
        //Handles null values and empty input gracefully.
        if (!numbers.Any())
        {
            return null; 
        }
        //Using Where to filter null values before calculating the average
        var nonNullNumbers = numbers.Where(x => x.HasValue);
        return nonNullNumbers.Any() ? nonNullNumbers.Average(x => x.Value) : (double?)null;
    }
}


public class BadCodeExample
{
    public static double CalculateAverage(List<double> numbers)
    {
        double sum = 0;
        foreach (double number in numbers)
        {
            sum += number;
        }
        return sum / numbers.Count; //Potential DivideByZeroException
    }

    public static void Main(string[] args)
    {
        List<double> numbers = new List<double> { 1, 2, 3, 4, 5 };
        double average = CalculateAverage(numbers);
        Console.WriteLine(average);

        List<double> emptyList = new List<double>();
        double average2 = CalculateAverage(emptyList); //Throws DivideByZeroException
        Console.WriteLine(average2); //This line will not be reached.
    }
}
