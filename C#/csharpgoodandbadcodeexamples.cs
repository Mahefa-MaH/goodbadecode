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


    public static Dictionary<string, int> CountWordFrequencies(string text)
    {
        if (string.IsNullOrEmpty(text))
        {
            return new Dictionary<string, int>();
        }

        return text.ToLower()
                   .Split(new char[] { ' ', ',', '.', '!', '?' }, StringSplitOptions.RemoveEmptyEntries)
                   .GroupBy(word => word)
                   .ToDictionary(group => group.Key, group => group.Count());

    }


    public static void Main(string[] args)
    {
        var numbers = new List<double> { 1.0, 2.5, 3.7, 4.2, 5.9 };
        Console.WriteLine($"Average: {CalculateAverage(numbers)}");

        string text = "This is a sample text. This text is used to test word frequencies.";
        var wordFrequencies = CountWordFrequencies(text);
        Console.WriteLine("Word Frequencies:");
        foreach (var kvp in wordFrequencies)
        {
            Console.WriteLine($"{kvp.Key}: {kvp.Value}");
        }


    }
}


public class BadCodeExample
{
    public static double CalculateAverage(List<double> numbers) //Missing Null and Empty check.  Using List instead of IEnumerable limits flexibility.
    {
        double sum = 0;
        foreach (double number in numbers)
        {
            sum += number;
        }
        return sum / numbers.Count; //Potential DivideByZeroException
    }

    public static Dictionary<string, int> CountWordFrequencies(string text) //No error handling, inefficient string manipulation.
    {
        Dictionary<string, int> wordCounts = new Dictionary<string, int>();
        string[] words = text.Split(' ');
        foreach (string word in words)
        {
            if (wordCounts.ContainsKey(word))
            {
                wordCounts[word]++;
            }
            else
            {
                wordCounts[word] = 1;
            }
        }
        return wordCounts;
    }

    public static void Main(string[] args)
    {
        List<double> numbers = new List<double> { 1, 2, 3, 4, 5 };
        Console.WriteLine(CalculateAverage(numbers)); //Potential exception if numbers is empty

        string text = "This is a test string";
        Dictionary<string, int> counts = CountWordFrequencies(text);
        foreach (KeyValuePair<string, int> kvp in counts)
        {
            Console.WriteLine(kvp.Key + ": " + kvp.Value);
        }
    }
}
