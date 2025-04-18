// Good Code: Using generics for type safety and efficiency.
public class GoodCodeExample<T> where T : IComparable<T>
{
    public T FindMax(T[] array)
    {
        if (array == null || array.Length == 0)
        {
            throw new ArgumentException("Array cannot be null or empty.");
        }
        T max = array[0];
        for (int i = 1; i < array.Length; i++)
        {
            if (array[i].CompareTo(max) > 0)
            {
                max = array[i];
            }
        }
        return max;
    }
}


// Bad Code: Lack of error handling and inefficient string concatenation.
public class BadCodeExample
{
    public string ConcatenateStrings(string[] strings)
    {
        string result = ""; //Inefficient way to concatenate strings.
        foreach (string str in strings)
        {
            result += str; // Inefficient string concatenation.
        }
        return result; // No error handling for null or empty input.

    }
}
