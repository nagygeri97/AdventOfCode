#include <bits/stdc++.h>

using namespace std;

int64_t concat(int64_t left, int64_t right) {
    stringstream ss;
    ss << left << right;
    return stol(ss.str());
}

template<bool AllowConcat>
bool isPossibleHelper(int64_t current, size_t index, const std::vector<int64_t>& numbers, int64_t target) {
    if (index == numbers.size()) {
        return current == target;
    }

    if (current > target) {
        return false;
    }

    if (isPossibleHelper<AllowConcat>(current + numbers[index], index + 1, numbers, target)) {
        return true;
    }

    if (isPossibleHelper<AllowConcat>(current * numbers[index], index + 1, numbers, target)) {
        return true;
    }

    if (AllowConcat) {
        if (isPossibleHelper<AllowConcat>(concat(current, numbers[index]), index + 1, numbers, target)) {
            return true;
        }
    }

    return false;
}

template<bool AllowConcat>
bool isPossible(int64_t total, const std::vector<int64_t>& numbers) {
    if (numbers.empty()) return false;
    return isPossibleHelper<AllowConcat>(numbers[0], 1, numbers, total);
}

int main() {
    string line;
    int64_t result = 0;
    int64_t result2 = 0;
    while(getline(cin, line)) {
        int64_t total;
        std::vector<int64_t> numbers;

        std::istringstream iss(line);
        std::string token;

        std::getline(iss, token, ':');
        total = std::stol(token);

        while (iss >> token) {
            numbers.push_back(std::stol(token));
        }

        if (isPossible<false>(total, numbers)) {
            result += total;
            result2 += total;
        }
        else if (isPossible<true>(total, numbers)) {
            result2 += total;
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}