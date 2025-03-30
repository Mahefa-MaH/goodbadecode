**Title:** Swift String Manipulation: Efficient vs. Inefficient Approaches

**Summary:**  Efficient Swift string manipulation leverages built-in methods and avoids unnecessary intermediate copies, whereas inefficient approaches can lead to significant performance degradation, especially with large strings.

**Good Code:**

```swift
import Foundation

func efficientStringManipulation(originalString: String, substring: String) -> String {
    // Check for substring existence before performing operations.
    guard originalString.contains(substring) else { return originalString }

    // Use range-based replacement for efficiency.
    if let range = originalString.range(of: substring) {
        return originalString.replacingOccurrences(of: substring, with: "replacement", options: [], range: range)
    }
    return originalString
}


func efficientStringConcatenation(strings: [String]) -> String {
    // Use joined() for efficient concatenation of arrays of strings.
    return strings.joined()
}


// Example usage
let myString = "This is a sample string with a substring."
let substring = "substring"
let efficientResult = efficientStringManipulation(originalString: myString, substring: substring)
print("Efficient result: \(efficientResult)") // Output: Efficient result: This is a sample string with a replacement.

let stringArray = ["Hello", " ", "World", "!"]
let efficientConcatenatedString = efficientStringConcatenation(strings: stringArray)
print("Efficient concatenated string: \(efficientConcatenatedString)") // Output: Efficient concatenated string: Hello World!


```

**Bad Code:**

```swift
import Foundation

func inefficientStringManipulation(originalString: String, substring: String) -> String {
    //Inefficient approach using multiple string creations and replacements.
    var modifiedString = originalString
    if modifiedString.contains(substring) {
      modifiedString = modifiedString.replacingOccurrences(of: substring, with: "replacement") // Replaces all occurrences, not just the first.

        // Unnecessary string creation
        modifiedString = "Modified: " + modifiedString

    }
    return modifiedString

}


func inefficientStringConcatenation(strings: [String]) -> String {
    //Inefficient concatenation using the + operator repeatedly.
    var result = ""
    for str in strings {
        result += str
    }
    return result
}


// Example Usage (will produce the same output but less efficiently)
let myStringBad = "This is a sample string with a substring."
let substringBad = "substring"
let inefficientResult = inefficientStringManipulation(originalString: myStringBad, substring: substringBad)
print("Inefficient result: \(inefficientResult)") // Output: Inefficient result: Modified: This is a sample string with a replacement.


let stringArrayBad = ["Hello", " ", "World", "!"]
let inefficientConcatenatedString = inefficientStringConcatenation(strings: stringArrayBad)
print("Inefficient concatenated string: \(inefficientConcatenatedString)") // Output: Inefficient concatenated string: Hello World!
```

**Key Takeaways:**

* **Avoid unnecessary string copying:** The good code minimizes the number of times a string is copied, reducing memory usage and improving performance.  The bad code creates multiple intermediate strings, leading to increased memory allocation and deallocation overhead.
* **Utilize built-in methods:** Swift's built-in string methods like `replacingOccurrences(of:with:options:range:)` and `joined()` are highly optimized.  Using them avoids the performance overhead of manual string manipulation.
* **Range-based string replacement:** Replacing substrings within a specific range is more efficient than replacing all occurrences when you only need to modify a particular part of the string.
* **Efficient String Concatenation:** The `joined()` method is significantly faster than repeated string concatenation using the `+` operator, especially for larger arrays.  The `+` operator creates new string instances with each concatenation.
* **Conditional Checks:**  Always check for the existence of a substring before attempting to replace it to avoid unnecessary processing.


The good code demonstrates best practices for Swift string manipulation, resulting in cleaner, more efficient, and more readable code.  The bad code highlights common pitfalls that can lead to performance issues and less maintainable code.
