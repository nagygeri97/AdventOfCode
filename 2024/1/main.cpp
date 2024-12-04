#include <bits/stdc++.h>

using namespace std;

int main() {
    vector<int> a, b;
    int x, y;
    while (cin >> x >> y) {
        a.push_back(x);
        b.push_back(y);
    }
    sort(a.begin(), a.end());
    sort(b.begin(), b.end());

    int result = 0;
    for (int i = 0; i < a.size(); ++i) {
        result += abs(a[i] - b[i]);
    }
    cout << result << endl;

    map<int, int> m;
    for (auto e : b) {
        ++m[e];
    }

    result = 0;
    for (auto e : a) {
        result += e * m[e];
    }
    cout << result << endl;
}