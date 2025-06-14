// Good Code: Using generics and LINQ for efficient and reusable code.
public static class GoodCodeExample
{
    public static List<T> FilterItems<T>(List<T> items, Func<T, bool> predicate)
    {
        return items.Where(predicate).ToList();
    }
}


// Bad Code:  Inefficient, hardcoded, and difficult to maintain.
public static class BadCodeExample
{
    public static List<int> FilterEvenNumbers(List<int> numbers)
    {
        List<int> evenNumbers = new List<int>();
        foreach (int number in numbers)
        {
            if (number % 2 == 0)
            {
                evenNumbers.Add(number);
            }
        }
        return evenNumbers;
    }
}
