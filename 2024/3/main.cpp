#include <bits/stdc++.h>

using namespace std;

int main() {
    stringstream buff;
    buff << cin.rdbuf();
    string text = buff.str();

    regex pattern(R"(mul\((\d+),(\d+)\))");

    int result = 0;
    for (sregex_iterator it(text.begin(), text.end(), pattern), end; it != end; ++it) {
        result += stoi(it->str(1)) * stoi(it->str(2));
    }
    cout << result << endl;

    regex pattern2(R"(mul\((\d+),(\d+)\)|do(n't)?\(\))");

    bool d = true;
    result = 0;
    for (sregex_iterator it(text.begin(), text.end(), pattern2), end; it != end; ++it) {
        if (it->str() == "do()") {
            d = true;
        } else if (it->str() == "don't()") {
            d = false;
        } else if (d) {
            result += stoi(it->str(1)) * stoi(it->str(2));
        }
    }
    cout << result << endl;
}