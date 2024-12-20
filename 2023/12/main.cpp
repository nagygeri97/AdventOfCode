#include <bits/stdc++.h>

using namespace std;

int64_t solve(const string_view& pattern, const vector<int64_t>& counts, int64_t i, map<pair<string_view, int64_t>, int64_t>& cache) {
    if (cache.contains({pattern, i})) {
        return cache[{pattern, i}];
    }

    if (i >= counts.size() && pattern.size() == 0) {
        return 1;
    }

    int64_t result = 0;
    if (pattern[0] == '.' || pattern[0] == '?') {
        result += solve(pattern.substr(1), counts, i, cache);
    } 
    
    if ((pattern[0] == '#' || pattern[0] == '?') && i < counts.size()) {
        int64_t count = counts[i];
        if (count <= pattern.size()) {
            bool success = true;
            for (int64_t j = 0; j < count; ++j) {
                if (pattern[j] == '.') {
                    success = false;
                    break;
                }
            }
            if (count < pattern.size()) {
                success &= pattern[count] == '.' || pattern[count] == '?';
                ++count;
            }
            if (success) {
                result += solve(pattern.substr(count), counts, i+1, cache);
            }
        }
    }

    cache[{pattern, i}] = result;
    return result;
}

int main() {
    string pattern; 
    string countsStr;
    int64_t result = 0;
    int64_t result2 = 0;
    while (cin >> pattern >> countsStr) {
        vector<int64_t> counts;
        stringstream ss(countsStr);
        string numStr;
        while (getline(ss, numStr, ',')) {
            counts.emplace_back(stoi(numStr));
        }
        map<pair<string_view, int64_t>, int64_t> cache;
        result += solve(pattern, counts, 0, cache);

        cache.clear();
        string pattern5 = pattern;
        vector<int64_t> counts5 = counts;
        for (int i = 1; i < 5; ++i) {
            pattern5 += "?"s + pattern;
            for (auto count : counts) {
                counts5.emplace_back(count);
            }
        }

        result2 += solve(pattern5, counts5, 0, cache);
    }
    cout << result << endl;
    cout << result2 << endl;
}