// Good Code: Using generics and LINQ for efficient and readable code.
public class GoodCodeExample<T> where T : IComparable<T>
{
    public T FindMax(IEnumerable<T> collection) => collection.Max();
    public IEnumerable<T> Sort(IEnumerable<T> collection) => collection.OrderBy(x => x);
}


//Bad Code:  Inefficient and hard to read. Lacks error handling.
public class BadCodeExample
{
    public int FindMax(int[] arr)
    {
        int max = arr[0];
        for (int i = 1; i < arr.Length; i++)
        {
            if (arr[i] > max)
                max = arr[i];
        }
        return max;
    }
    public int[] Sort(int[] arr)
    {
        int n = arr.Length;
        for (int i = 0; i < n - 1; i++)
            for (int j = 0; j < n - i - 1; j++)
                if (arr[j] > arr[j + 1])
                {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
        return arr;

    }
}
