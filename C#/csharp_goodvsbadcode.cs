// Good Code: Uses generics for type safety and efficiency.  Handles potential exceptions.
public static class GoodCodeExample
{
    public static T FindMax<T>(IEnumerable<T> collection) where T : IComparable<T>
    {
        if (collection == null || !collection.Any())
        {
            throw new ArgumentException("Collection cannot be null or empty.");
        }

        T max = collection.First();
        foreach (T item in collection)
        {
            if (item.CompareTo(max) > 0)
            {
                max = item;
            }
        }
        return max;
    }
}


// Bad Code: Lacks type safety, error handling, and efficiency.  Uses unnecessary boxing/unboxing.
public static class BadCodeExample
{
    public static object FindMax(object[] array)
    {
        if (array == null || array.Length == 0) return null; //Poor error handling

        object max = array[0];
        foreach (object item in array)
        {
            if (((IComparable)item).CompareTo(max) > 0)
            {
                max = item;
            }
        }
        return max;
    }
}
