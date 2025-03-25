#include <iostream>
#include <vector>

// Good Code Example:  Using a function to calculate the sum of vector elements.
int sumVector(const std::vector<int>& vec) {
  int sum = 0;
  for (int num : vec) {
    sum += num;
  }
  return sum;
}

int main() {
  std::vector<int> numbers = {1, 2, 3, 4, 5};
  int total = sumVector(numbers);
  std::cout << "Sum: " << total << std::endl;
  return 0;
}


// Bad Code Example: Calculating the sum directly in main, without a function, and less readable.
int main2() {
    int sum = 0;
    int arr[] = {1,2,3,4,5,6,7,8,9,10};
    for(int i = 0; i < 10; ++i){
        sum += arr[i];
    }
    std::cout << "Sum: " << sum << std::endl;
    return 0;
}
