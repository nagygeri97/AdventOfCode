#include <bits/stdc++.h>

using namespace std;

int myHash(const string& s) {
    int currentValue = 0;
    for (char c : s) {
        currentValue += static_cast<int>(c);
        currentValue *= 17;
        currentValue %= 256;
    }
    return currentValue;
}

class HashMap {
public:
    void insert(string key, int val) {
        if (m.contains(key)) {
            m[key].second = val;
        } else {
            m[key] = {lastFree++, val};
        }
    }

    void erase(string key) {
        if (m.contains(key)) {
            m.erase(key);
        }
    }

    vector<int> getRawMap() {
        vector<pair<int, int>> v;
        v.reserve(m.size());
        for (auto [k, p] : m) {
            v.push_back(p);
        }

        sort(v.begin(), v.end());

        vector<int> result;
        result.reserve(v.size());
        for (auto [pos, val] : v) {
            result.push_back(val);
        }
        return result;
    }
private:
    int lastFree = 0;
    unordered_map<string, pair<int, int>> m;
};

int main() {
    string instruction;
    int result = 0;
    vector<HashMap> hm(256);
    while (getline(cin, instruction, ',')) {
        result += myHash(instruction);

        string label;
        int i = 0;
        while (isalpha(instruction[i])) {
            label += instruction[i];
            ++i;
        }

        int pos = myHash(label);

        if (instruction[i] == '=') {
            ++i;
            string valStr;
            while (i < instruction.size()) {
                valStr += instruction[i];
                ++i;
            }
            int val = stoi(valStr);
            hm[pos].insert(label, val);
        } else {
            hm[pos].erase(label);
        }
    }
    cout << result << endl;

    int result2 = 0;
    for (int i = 0; i < 256; ++i) {
        auto v = hm[i].getRawMap();
        for (int j = 0; j < v.size(); ++j) {
            result2 += (i+1) * (j+1) * v[j];
        }
    }
    cout << result2 << endl;
}