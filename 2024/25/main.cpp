#include <bits/stdc++.h>

using namespace std;

int main() {
    vector<vector<int>> keys;
    vector<vector<int>> locks;

    string tmp;
    bool newBlock = true;
    vector<int> counts(5, 0);
    bool isKey = false;
    while (getline(cin, tmp)) {
        if (tmp.empty()) {
            newBlock = true;
            if (isKey) {
                keys.push_back(counts);
            } else {
                locks.push_back(counts);
            }
            counts = vector<int>(5, 0);
            continue;
        }

        if (newBlock) {
            isKey = tmp[0] == '.';
            newBlock = false;
        }
        for (int i = 0; i < 5; ++i) {
            if (tmp[i] == '#') ++counts[i];
        }
    }
    if (isKey) {
        keys.push_back(counts);
    } else {
        locks.push_back(counts);
    }

    int result = 0;
    for (const auto& key : keys) {
        for (const auto& lock : locks) {
            bool good = true;
            for (int i = 0; i < 5; ++i) {
                if (key[i] + lock[i] > 7) {
                    good = false;
                }
            }
            if (good) ++result;
        }
    }
    cout << result << endl;
}