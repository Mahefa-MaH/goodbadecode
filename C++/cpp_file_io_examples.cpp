#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// Good Code Example: Reading a file and processing its lines
std::vector<std::string> readFile(const std::string& filename) {
    std::ifstream file(filename);
    std::vector<std::string> lines;
    std::string line;
    if (file.is_open()) {
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        //Handle error appropriately, maybe throw an exception
    }
    return lines;
}

int main() {
    auto lines = readFile("myfile.txt");
    for (const auto& line : lines) {
        std::cout << line << std::endl;
    }
    return 0;
}


//Bad Code Example:  Error prone file handling and memory management
int badFileHandling() {
    FILE *fp;
    char line[255];
    fp = fopen("myfile.txt", "r");
    if(fp != NULL){
        while(fgets(line, sizeof(line), fp) != NULL){
            printf("%s", line);
        }
        fclose(fp);
    }
    return 0;
}

