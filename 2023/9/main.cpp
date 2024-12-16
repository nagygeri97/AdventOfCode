#include <bits/stdc++.h>

using namespace std;

int solve(const vector<int>& v) {
    if (all_of(v.begin(), v.end(), [](int x){ return x == 0; })) {
        return 0;
    }

    vector<int> diffs;
    diffs.reserve(v.size() - 1);
    for (int i = 1; i < v.size(); ++i) {
        diffs.emplace_back(v[i] - v[i-1]);
    }
    return v.back() + solve(diffs);
}

int main() {
    string tmp;
    int result = 0;
    int result2 = 0;
    while(getline(cin, tmp)) {
        stringstream ss(tmp);
        int tmpN;
        vector<int> v;
        while (ss >> tmpN) {
            v.emplace_back(tmpN);
        }
        result += solve(v);
        reverse(v.begin(), v.end());
        result2 += solve(v);
    }
    cout << result << endl;
    cout << result2 << endl;
}