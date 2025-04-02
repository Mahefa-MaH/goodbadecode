#include <iostream>
#include <vector>
#include <algorithm>

// Good Code: Uses move semantics for efficiency.
void goodMove(std::vector<std::string>&& vec) {
  for (auto&& str : vec) {
    std::cout << str << std::endl;
  }
}

// Bad Code: Unnecessary copying.
void badMove(const std::vector<std::string>& vec) {
    std::vector<std::string> copy = vec; //Unnecessary copy
    for (const auto& str : copy) {
        std::cout << str << std::endl;
    }
}

int main() {
  std::vector<std::string> strings = {"Hello", "World", "Move", "Semantics"};
  goodMove(std::move(strings)); //Move the vector


  std::vector<std::string> strings2 = {"Hello", "Again", "More", "Moves"};
  badMove(strings2); //Copy the vector

  return 0;
}
