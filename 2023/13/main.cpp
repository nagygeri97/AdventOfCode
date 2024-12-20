#include <bits/stdc++.h>

using namespace std;

int64_t solveHorizontal(const vector<string>& pattern, optional<int64_t> forbiddenRow = nullopt) {
    for (int i = 1; i < pattern.size(); ++i) {
        if (forbiddenRow.has_value() && i == *forbiddenRow) continue;
        bool success = true;
        for (int offset = 0; i + offset < pattern.size() && i - 1 - offset >= 0; ++offset) {
            if (pattern[i + offset] != pattern[i - 1 - offset]) {
                success = false;
                break;
            }
        }
        if (success) {
            return i;
        }
    }
    return 0;
}

int64_t solve(const vector<string>& pattern, optional<int64_t> forbiddenScore = nullopt) {
    int64_t result = 100 * solveHorizontal(pattern, forbiddenScore.has_value() && *forbiddenScore >= 100 ? make_optional<int64_t>(*forbiddenScore / 100) : nullopt);
    if (result > 0) return result;

    vector<string> transposedPattern;
    for (int i = 0; i < pattern[0].size(); ++i) {
        string tmp;
        for (int j = 0; j < pattern.size(); ++j) {
            tmp += pattern[j][i];
        }
        transposedPattern.push_back(tmp);
    }
    return solveHorizontal(transposedPattern, forbiddenScore.has_value() && *forbiddenScore < 100 ? forbiddenScore : nullopt);
}

char flip(char c) {
    if (c == '.') return '#';
    else return '.';
}

int64_t solve2(vector<string> pattern) {
    int64_t wrong = solve(pattern);
    for (auto& row : pattern) {
        for (auto& c : row) {
            c = flip(c);
            int64_t result = solve(pattern, wrong);
            if (result > 0) return result;
            c = flip(c);
        }
    }
    return 0;
}

int main() {
    string row;
    vector<string> pattern;
    int64_t result = 0;
    int64_t result2 = 0;
    while (getline(cin, row)) {
        if (row.size() == 0) {
            result += solve(pattern);
            result2 += solve2(pattern);
            pattern.clear();
            continue;
        }
        pattern.push_back(row);
    }
    result += solve(pattern);
    result2 += solve2(pattern);

    cout << result << endl;
    cout << result2 << endl;
}