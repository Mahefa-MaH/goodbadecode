// Good Code Example:  Using a clear, concise function to calculate the factorial.
class Factorial {
    public static function calculate(n:Int):Int {
        if (n <= 1) return 1;
        return n * calculate(n - 1);
    }
}

// Bad Code Example:  Unnecessary complexity and unclear logic for the same task.
class FactorialBad {
    public static function calculateBad(n:Int):Int {
        var result:Int = 1;
        var i:Int = n;
        while (i > 1) {
            result *= i;
            i--;
            if (i == 0) break; //Redundant break statement
        }
        return result;
    }
}


class Main {
    static public function main() {
        trace(Factorial.calculate(5)); // Output: 120
        trace(FactorialBad.calculateBad(5)); // Output: 120
    }
}
