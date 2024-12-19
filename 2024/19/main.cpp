#include <bits/stdc++.h>

using namespace std;

int64_t possibleCount(const string_view& tmp, const vector<string>& patterns, unordered_map<string_view, int64_t>& cache) {
    if (tmp.size() == 0) {
        return 1;
    }
    if (cache.contains(tmp)) {
        return cache[tmp];
    }
    int64_t result = 0;
    for (const string& pattern : patterns) {
        if (tmp.starts_with(pattern)) {
            result += possibleCount(tmp.substr(pattern.size()), patterns, cache);
        }
    }
    cache[tmp] = result;
    return result;
}

int main() {
    string tmp;
    getline(cin, tmp);
    for (char& c: tmp) {
        if (c == ',') {
            c = ' ';
        }
    }
    stringstream ss(tmp);
    vector<string> patterns;
    while(ss >> tmp) {
        patterns.emplace_back(tmp);
    }
    getline(cin, tmp);
    int64_t result = 0;
    int64_t result2 = 0;
    unordered_map<string_view, int64_t> cache;
    while (getline(cin, tmp)) {
        int64_t count = possibleCount(tmp, patterns, cache);
        if (count > 0) {
            ++result;
            result2 += count;
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}