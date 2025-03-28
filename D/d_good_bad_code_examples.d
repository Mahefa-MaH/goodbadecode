// Good Code Example: Calculating the factorial of a number using recursion.
long factorialGood(long n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorialGood(n - 1);
    }
}


// Bad Code Example: Calculating the factorial of a number with potential stack overflow.  No input validation or error handling.
long factorialBad(long n) {
    return n * factorialBad(n-1);
}

// Good Code Example:  Finding the maximum element in an array.  Handles empty arrays gracefully.
long maxGood(long[] arr) {
    if (arr.length == 0) {
        return -1; // Or throw an exception, depending on requirements.
    }
    long maxVal = arr[0];
    foreach (long val; arr) {
        if (val > maxVal) {
            maxVal = val;
        }
    }
    return maxVal;
}


// Bad Code Example: Finding the maximum element in an array.  Does not handle empty arrays, leading to runtime errors.  Inefficiently checks against itself.
long maxBad(long[] arr) {
    long maxVal = arr[0];
    foreach(long val; arr){
        if(val > arr[0]) maxVal = val;
    }
    return maxVal;
}


//Good Code Example: Function to reverse a string
string reverseGood(string s){
    return reverse(s);
}

//Bad Code Example: Function to reverse a string, inefficient and complicated.
string reverseBad(string s){
    string reversed = "";
    for(int i = s.length -1; i >=0; i--){
        reversed ~= s[i];
    }
    return reversed;
}
