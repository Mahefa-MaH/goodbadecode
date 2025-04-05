// Good Code: Using generics for type safety and flexibility
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


// Bad Code: Lack of error handling and inefficient approach
public class BadCodeExample
{
    public int FindMax(int[] array)
    {
        int max = array[0]; //Potential IndexOutOfRangeException
        for (int i = 1; i < array.Length; i++)
        {
            if (array[i] > max)
            {
                max = array[i];
            }
        }
        return max;
    }
}

//Demonstrates usage with error handling for GoodCodeExample
public void GoodCodeDemo() {
    GoodCodeExample<int> goodCode = new GoodCodeExample<int>();
    try{
        int[] numbers = { 1, 5, 2, 8, 3 };
        int max = goodCode.FindMax(numbers);
        Console.WriteLine($"Max: {max}"); //Output: Max 8

        int[] emptyArray = {};
        max = goodCode.FindMax(emptyArray); //Throws exception
    } catch (ArgumentException ex){
        Console.WriteLine($"Error: {ex.Message}"); //Output: Error: Array cannot be null or empty.
    }
}


//Demonstrates usage and potential exception for BadCodeExample
public void BadCodeDemo(){
    BadCodeExample badCode = new BadCodeExample();
    try{
        int[] numbers = {1,5,2,8,3};
        int max = badCode.FindMax(numbers);
        Console.WriteLine($"Max: {max}"); //Output: Max 8

        int[] emptyArray = {};
        max = badCode.FindMax(emptyArray); //Throws exception
    } catch (IndexOutOfRangeException ex){
        Console.WriteLine($"Error: {ex.Message}"); //Output: Error: Index was outside the bounds of the array.
    }
}
