**Title:** Efficient String Manipulation in Perl: Good vs. Bad Practices

**Summary:**  The key difference lies in utilizing Perl's built-in string operators for efficiency and readability versus employing less efficient looping constructs and manual character manipulation.  This impacts both performance and code maintainability.

**Good Code:**

```perl
use strict;
use warnings;

my $string = "This is a sample string.";
my $substring = "sample";

# Efficiently check if a substring exists
if ($string =~ /$substring/) {
    print "Substring found!\n";
} else {
    print "Substring not found.\n";
}


# Efficiently replace a substring
my $newString = $string =~ s/$substring/example/;
print "Replaced string: $newString\n";


#Efficiently extract a portion of the string using substr
my $extracted = substr($string, 10, 6); #Extract "sample" starting at index 10, length 6.
print "Extracted substring: $extracted\n";
```

**Bad Code:**

```perl
my $string = "This is a sample string.";
my $substring = "sample";
my $found = 0;

# Inefficient substring search using a loop
for (my $i = 0; $i <= length($string) - length($substring); $i++) {
    my $temp = substr($string, $i, length($substring));
    if ($temp eq $substring) {
        $found = 1;
        last;
    }
}

if ($found) {
    print "Substring found!\n";
} else {
    print "Substring not found.\n";
}

#Inefficient string replacement with looping and concatenation.
my $newString = "";
for my $i (0..length($string)-1){
    if(substr($string,$i,length($substring)) eq $substring){
        $newString .= "example";
        $i += length($substring)-1;
    } else {
        $newString .= substr($string,$i,1);
    }
}
print "Replaced String (Inefficient): $newString\n";
```

**Key Takeaways:**

* **Readability and Maintainability:** The good code is significantly more concise and easier to understand.  The bad code is verbose and harder to debug.
* **Efficiency:** Perl's built-in regular expressions (`=~`) and string functions (`substr`, `s///`) are highly optimized and far more efficient than manually iterating through strings. The Bad code's looping approach has O(n*m) complexity where n is the length of the string and m is the length of the substring, while regex is generally much faster.
* **Security:** While not directly demonstrated here, the good code's use of parameterized operations (regex) reduces the risk of injection vulnerabilities that could arise from manual string manipulation in the bad code, particularly if user-supplied data were involved.
* **Best Practices:**  The good code exemplifies the use of `use strict;` and `use warnings;` which are crucial for catching potential errors and improving code quality.  The bad code lacks these essential practices.
* **Correctness:** The bad code's replacement function has edge case issues and will produce incorrect results in some instances. The good code's `s///` operator handles these cases correctly.


