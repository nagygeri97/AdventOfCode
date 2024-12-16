#include <bits/stdc++.h>

using namespace std;

int main() {
    string dirs;
    getline(cin, dirs);
    
    string tmp;
    getline(cin, tmp);

    unordered_map<string, pair<string, string>> m;
    while (getline(cin, tmp)) {
        string key = tmp.substr(0, 3);
        string left = tmp.substr(7, 3);
        string right = tmp.substr(12, 3);
        m[key] = {left, right};
    }

    string curr = "AAA";
    int result = 0;
    int i = 0;
    while (curr != "ZZZ") {
        curr = dirs[i] == 'L' ? m[curr].first : m[curr].second;
        ++i;
        i %= dirs.size();
        ++result;
    }
    cout << result << endl;

    vector<int64_t> loops;
    for (const auto& [key, _] : m) {
        if (key.back() == 'A') {
            // currSet.emplace(key);
            string curr = key;
            int64_t offset = 0;
            i = 0;
            while (curr.back() != 'Z') {
                curr = dirs[i] == 'L' ? m[curr].first : m[curr].second;
                ++i;
                i %= dirs.size();
                ++offset;
            }
            loops.emplace_back(offset);
            // int loop = 0;
            // do {
            //     curr = dirs[i] == 'L' ? m[curr].first : m[curr].second;
            //     ++i;
            //     i %= dirs.size();
            //     ++loop;
            // } while (curr.back() != 'Z');
            // cout << offset << " + " << loop << "k" << endl;

            // Turns out Zs keep repeating with the same offset.
        }
    }

    int64_t result2 = loops[0];
    for (int i = 1; i < loops.size(); ++i) {
        int64_t next = result2 * loops[i] / gcd(result2, loops[i]);
        result2 = next;
    }
    cout << result2 << endl;
}